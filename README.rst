emacs-ci
========

Basic Continuous Integration (CI) for Emacs projects.

This can be used in any ELisp project on any CI host. The project itself need not be on a package archive like ELPA, and its external dependencies need not be on package archives either, as long as you specify where they can be found using a package recipe.

Emacs CI uses Straight.el to install packages and their dependencies using any recipes you specify. The specific CI steps also use package-lint and checkdoc (for linting), and ert-runner and undercover (for tests and coverage reports).

All of these steps take effect in the ``.emacs-ci/init/`` folder within project repo. As they do not interfere with system Emacs configuration and are fully self-contained, you can run these both locally as well as on CI services such as GitHub Actions.

How to Use It
-------------

Individual steps are explained with examples, below, and sample Makefiles and GitHub Actions worflows are provided at the end.

1. Clone Emacs CI
~~~~~~~~~~~~~~~~~

In your CI workflow, clone the repo into your repo's root path.

.. code-block:: bash

  git clone https://github.com/countvajhula/emacs-ci.git .emacs-ci

It's advisable to use a dot-prefixed name for the cloned folder so that its contents will be ignored by Emacs when it searches for ELisp modules in your project directory during initialization.

2. (Optional) Declare External Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have any external project dependencies that aren't listed on package archives, add ``straight-use-package`` declarations for them in a ``<repo-root>/.ci/ci-deps.el`` ELisp module that's checked into your repo. This module, if present, will be loaded before installing your project packages. For example:

.. code-block:: bash

  ;; Install any third-party packages required by the project
  ;; that aren't listed on package archives (if they are,
  ;; Emacs CI will find them there during the usual install step)
  (straight-use-package
   '(rigpa :host github :repo "countvajhula/rigpa" :type git))

  (provide 'ci-deps)

3. Declare Environment Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll need the following environment variables. Set them either in your Makefile that runs these scripts, or in the CI workflow file.

.. code-block:: bash

  export CI_PROJECT=<your-project-name>
  export CI_PACKAGES=<package-1> <package-2> ...
  export CI_REPO_HOST=<github/gitlab/etc>
  export CI_REPO_PATH=<your-account/your-project>

``CI_PROJECT`` can be left out if there's just one package (the most common case). If there are multiple packages, they are each expected to be in correspondingly-named folders at the top level of your repo. If you have just one package, the modules are expected to be directly at the top level of the repo, unless you also declare the ``CI_PROJECT`` variable, in which case it will look for the package in a folder of the same name at the top level.

If your packages are listed on package archives like ELPA, ``CI_REPO_HOST`` and ``CI_REPO_PATH`` tell Emacs CI the canonical locations of your packages, which allows Straight to validate them.

4. Run the CI Modules You Need
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emacs CI includes the following modules:

- ``bootstrap``
- ``install``
- ``build`` (byte-compile your project)
- ``native-build`` (native-compile your project)
- ``lint``
- ``checkdoc``
- ``test``
- ``coverage``

``bootstrap`` (which bootstraps Straight.el) always needs to be run first, before running any of the others that you might need. Each module (including bootstrap) may be run using Emacs's "batch mode." For example:

.. code-block:: bash

  cd .emacs-ci && emacs --batch --quick --load lint.el

Sample Makefiles and Workflows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Single-package project
``````````````````````

For a single-package project hosted on GitHub that uses all the CI steps including generating and submitting a coverage report to Coveralls, see Dynaring:

- `Makefile <https://github.com/countvajhula/dynaring/blob/master/Makefile>`__

- `GitHub Actions Workflow <https://github.com/countvajhula/dynaring/blob/master/.github/workflows/test.yml>`__

Multi-package project
`````````````````````

For a `multi-package project <https://drym-org.github.io/symex.el/Installing-Symex.html>`__, see Symex:

- `Makefile <https://github.com/drym-org/symex.el/blob/main/Makefile>`_

- `GitHub Actions Workflow <https://github.com/drym-org/symex.el/blob/main/.github/workflows/test.yml>`_

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
