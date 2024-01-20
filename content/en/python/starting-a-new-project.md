---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new Python project is basically setting up the building blocks and structures you need for your codebase. Why? It's about keeping things clean, organized, and easy-to-update.

## How to:

Here's a simple way to kickstart your Python project. 

```Python
# Create new project directory 
mkdir new_project
cd new_project

# Initiate Python virtual environment
python3 -m venv env

# Activation
source env/bin/activate
```
What you'll see is this:

```Output
(new_project) $
```
The prefix indicates that you're in the Python virtual environment. Now let's create the main script:

```Python
# Create new Python script
touch main.py

# Write simple Hello World code
echo "print('Hello, World!')" > main.py

# Run the Python script
python main.py
```

Here's your result:

```Output
Hello, World!
```

Great work! Your Python environment and project is up and running.

## Deep Dive

In the late 2000s, the Python community realized the need for project isolation. This led to the creation of virtual environments. There are alternatives like Docker and Pipenv, but Python's venv is lightweight and built-in, making it the first pick for many.

When initiating the project with venv, Python creates an isolated environment for your project, cloning your Python interpreter, pip, and other binaries into your project directory. This isolation helps manage dependencies and avoid version conflicts between different projects.

## See Also

1. Python's venv documentation: https://docs.python.org/3/library/venv.html
2. Docker: https://www.docker.com/what-docker
3. Pipenv: https://pipenv.pypa.io/en/latest/