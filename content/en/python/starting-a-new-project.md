---
title:                "Starting a new project"
html_title:           "Python recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project means creating the initial foundation for your code. Doing so allows programmers to plan out their code structure, design their data flow, and create an adaptable framework for future development.

## How to:
Let's start a new Python project. Here's a basic setup:

Directory structure:
```
project/
├── bin/
├── docs/
├── project/
│   └── __init__.py
├── tests/
│   └── __init__.py
├── setup.py
└── README.md
```
`__init__.py` tells Python that the directory should be treated as a package.

`setup.py`:
```python
from setuptools import setup, find_packages

setup(name='YourProject',
      version='0.1',
      description='Project Description',
      author='Your Name',
      packages=find_packages())
```

Run `python setup.py install` for installing package.

Initial test in `tests/test_initial.py`:
```python
def test_initial():
    assert 1 == 1
```

Run `pytest tests` for starting tests.

## Deep Dive
Historically, Python developers used folder structures, and setup files to manage their projects manually. Over time, tools like `setuptools` and `pip` showed up to help automate and standardize practices.

There are alternatives to manual project creation: cookiecutter provides project templates, and IDEs often include their own project management systems. But rolling your own can make sure your project structure suits your needs exactly.

Implementing a new project implies setting necessary directories, initializing your code base, and choosing your tech stack. This structure aids in managing source code, tests, and documentation, and helps other contributors get started easily.

## See Also
Some helpful links:
- [The Hitchhiker’s Guide to Python!](https://docs.python-guide.org/)
- [setuptools documentation](https://setuptools.readthedocs.io/)
- [cookiecutter GitHub](https://github.com/cookiecutter/cookiecutter)
- [pytest documentation](https://docs.pytest.org/)

Now you're all set to start your own Python project! Happy coding!