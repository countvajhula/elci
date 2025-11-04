elci
====

Self-contained Continuous Integration (CI) for Emacs Lisp projects.

.. contents:: :depth: 2

Elci can be used in any ELisp project to run code quality checks locally during development as well in automated CI workflows on any CI host.

Elci uses Straight.el to install packages and their dependencies using any recipes you specify. Your project itself need not be on a package archive like ELPA, and its external dependencies need not be on package archives either, as long as you specify where they can be found in their package recipes.

The specific CI steps also use ``package-lint`` and ``checkdoc`` (for linting), and ``ert-runner`` and ``undercover`` (for tests and coverage reports).

All of these steps take effect in the ``.elci/`` folder within project repo. As they do not interfere with system Emacs configuration and are fully self-contained, you can run these both locally as well as on CI services such as GitHub Actions.

How to Use It
-------------

Individual steps are explained with examples, below. These same steps may be performed locally during development or as part of CI workflows. Sample Makefiles, recipes, and GitHub Actions workflows are provided at the end.

1. Clone Elci
~~~~~~~~~~~~~

First, clone this repository into your project's root path.

.. code-block:: bash

  git clone https://github.com/countvajhula/elci.git .elci

It's advisable to use a dot-prefixed name for the cloned folder so that its contents will be ignored by Emacs when it searches for ELisp modules in your project directory during initialization.

2. Declare Package Recipes
~~~~~~~~~~~~~~~~~~~~~~~~~~

Elci finds and builds packages based on a single, declarative recipe file that you provide. This file is the source of truth for your project's structure and dependencies.

* **Location**: Create a file at ``<repo-root>/.ci/recipes.eld``.
* **Format**: The file should contain a single Lisp list of Straight.el-style package recipes.
* **Content**: The list should at a minimum include recipes for all packages within your repository. It could also include external dependencies that are not on standard package archives like MELPA, or whose standard recipe you want to override for any reason.
* **Local Packages**: For packages contained within your project's repository, use the special value ``:local-repo "."``.

For example, a single-package project might have a ``.ci/recipes.eld`` like this:

.. code-block:: emacs-lisp

   ;; .ci/recipes.eld for a single-package repo
   (
    (my-package :type git :local-repo "." :files ("*.el"))
   )

A multi-package suite would define a recipe for each of its components:

.. code-block:: emacs-lisp

   ;; .ci/recipes.eld for a multi-package suite
   (
    (my-core :type git :local-repo "." :files ("my-core/*.el"))
    (my-ui :type git :local-repo "." :files ("my-ui/*.el"))
    ;; A recipe for an external dependency that isn't
    ;; listed on a package archive such as MELPA
    (my-dependency :host github :repo "user/my-dependency")
   )

During the ``bootstrap`` step, this file is used to generate a local recipe repository called "XELPA" (eXtra ELPA), which becomes the primary source for package information during the CI run.

If you do not specify a recipe to use in your ``recipes.eld``, Straight will fall back to consulting common recipe repositories such as ELPA, NonGNU ELPA, and MELPA. You can always override the standard recipes using your ``recipes.eld``.

3. Declare Environment Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll need the following environment variables, typically set in your project ``Makefile`` or in the CI workflow specification:

.. code-block:: bash

  export CI_PACKAGES=<package-1> <package-2> ...
  export CI_PROJECT=<your-project-name>

``CI_PACKAGES``: The list of packages being developed in your repo. This tells the CI checks which packages to target, e.g., for building, linting, and testing. Most commonly, this is just a single package.

``CI_PROJECT`` (Optional): For multi-package projects, this project name is used as the common symbol prefix to validate against in lint checks. It can be left out if there's just one package in your repo (the most common case).

4. Run the CI Modules You Need
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Elci includes the following modules:

- ``bootstrap``: Initializes the CI environment by bootstrapping Straight.el and generating the local XELPA recipe repository. **Must be run first.**
- ``install``: Installs all project packages and their dependencies.
- ``byte-compile``: Byte-compiles the project's packages.
- ``native-compile``: Native-compiles the project's packages (on supported Emacs versions).
- ``lint``: Runs ``package-lint`` on the source files.
- ``checkdoc``: Runs ``checkdoc`` on the source files.
- ``test``: Runs the project's ERT tests.
- ``coverage``: Runs tests and generates a code coverage report using ``undercover``.

Each module is run in a clean, isolated Emacs process. For example:

.. code-block:: bash

  cd .elci && emacs --batch --quick --load lint.el

Sample Makefiles, Recipes, and Workflows
----------------------------------------

Single-package project
~~~~~~~~~~~~~~~~~~~~~~

For a single-package project hosted on GitHub that uses all the CI steps including generating and submitting a coverage report to `Coveralls <https://coveralls.io/>`_, see Dynaring:

- `Makefile <https://github.com/countvajhula/dynaring/blob/master/Makefile>`__

- `recipes.eld <https://github.com/countvajhula/dynaring/blob/master/.ci/recipes.eld>`__

- `GitHub Actions Workflow <https://github.com/countvajhula/dynaring/blob/master/.github/workflows/test.yml>`__

Multi-package project
~~~~~~~~~~~~~~~~~~~~~

For a `multi-package project <https://drym-org.github.io/symex.el/Installing-Symex.html>`__, see Symex:

- `Makefile <https://github.com/drym-org/symex.el/blob/main/Makefile>`_

- `recipes.eld <https://github.com/drym-org/symex.el/blob/main/.ci/recipes.eld>`__

- `GitHub Actions Workflow <https://github.com/drym-org/symex.el/blob/main/.github/workflows/test.yml>`_

Troubleshooting
---------------

Bootstrap Fails
~~~~~~~~~~~~~~~

A failure at the bootstrap stage is usually an indication of a problem with ``recipes.eld``.

* **Symptom**: ``Error: wrong-type-argument (listp my-package)``
* **Cause**: ``recipes.eld`` is expected to contain a *list* of recipes. Recipes written directly, without a containing list, could cause this error.
* **Solution**: Wrap the recipe(s) in ``recipes.eld`` in a list (see above for examples).

Missing Files
~~~~~~~~~~~~~

* **Symptom**: ``No such file or directory``
* **Cause**: A package used in your CI workflow is expecting a file to be present and not finding it. Typically, this is due to a problem in the package recipe and not including the correct files via ``:files``. This commonly occurs with third party dependencies where you may not know the recipe for correctly building the package.
* **Solution**: If the package is listed on a public recipe repository and you are overriding it, you could look at its recipe there to get an idea. Otherwise, use a reasonable recipe as a starting point and make appropriate changes to ``:files`` in response to the reported errors, until it works.

Coverage Report is Empty or Incomplete
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* **Symptom**: The tests run and pass, but the generated coverage report is empty or shows 0% coverage for some files.
* **Cause**: ``undercover.el`` needs to instrument your source (``.el``) files. However, the ``build`` step creates compiled (``.elc``) files, and Emacs will prefer to load these faster files during the test run, bypassing the instrumentation.
* **Solution**: The ``coverage.el`` script handles this automatically by telling its subprocess to prefer loading ``.el`` files over ``.elc`` files. If you are still having issues, ensure your ``UNDERCOVER_CONFIG`` in your ``Makefile`` is pointing to the correct source files (e.g., ``"*.el"`` for a single-package repo).

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
