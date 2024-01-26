---
title:                "Starting a new project"
date:                  2024-01-20T18:04:06.901675-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project is all about creating a fresh directory with files set up for your new code adventure. It's like breaking ground on a building site but for programmers. We do this to transform ideas into working software, organize our code, and manage complexity from the get-go.

## How to:

Let's start a Python project. First up, make a new directory:

```bash
mkdir my_new_project
cd my_new_project
```

Now, set up a virtual environment - this keeps our project's dependencies nice and tidy:

```bash
python -m venv venv
source venv/bin/activate # On Windows, use `venv\Scripts\activate`
```

With our virtual land prepped, sow the seeds of your project with a `main.py` file:

```bash
touch main.py
echo "print('Hello, new project!')" > main.py
python main.py
```

Output:
```plaintext
Hello, new project!
```

For good measure, let’s pin down the dependencies early. Even if none exist yet:

```bash
pip freeze > requirements.txt
```

And that's the embryo of your project. From here, it grows.

## Deep Dive

In the past, many a programmer would just wing it, starting code in a solitary file. Chaos would often ensue as the project grew. Nowadays, we've got better practices.

For Python, we've got conventions like PEP 8 for style guidelines. There are also tools like `cookiecutter` which create projects from templates. Want a web app? There's a template for that. It's set up to save you time.

On the other hand, you might like to do it manually, as we showed above. This method gives you total control, building your project from scratch. Just remember to keep track of dependencies with `requirements.txt`. It's crucial for when you share your project or deploy it.

## See Also

- [The Hitchhiker's Guide to Python](https://docs.python-guide.org/) - An opinionated guide on best practices in Python.
- [PEP 8 -- Style Guide for Python Code](https://peps.python.org/pep-0008/) - The style Bible for Python developers.
- [Cookiecutter](https://github.com/cookiecutter/cookiecutter) - A command-line utility to create projects from templates.
