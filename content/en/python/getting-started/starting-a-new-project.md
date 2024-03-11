---
changelog:
- 2024-02-22, dogweather, reviewed
date: 2024-02-22 14:52:40.960064-07:00
description: "Starting a new project in Python is about setting up a structured, maintainable\
  \ framework from the outset. Programmers do this to ensure that their code\u2026"
lastmod: '2024-03-11T00:14:33.558160-06:00'
model: gpt-4-0125-preview
summary: "Starting a new project in Python is about setting up a structured, maintainable\
  \ framework from the outset. Programmers do this to ensure that their code\u2026"
title: Starting a new project
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Python is about setting up a structured, maintainable framework from the outset. Programmers do this to ensure that their code is easy to read, debug, and collaborate on, especially as the project and the team working on it grow over time.

## How to:

### Create a Virtual Environment
A virtual environment is a self-contained directory that contains all the necessary executables to use the packages that a Python project would need. It is advisable to create a virtual environment for each project to avoid conflicts between project dependencies. Use the `venv` module, which is part of the standard Python library.

```shell
# Replace 'myproject' with the name of your project
python3 -m venv myproject-env
```

To activate the virtual environment:

On Windows:
```shell
myproject-env\Scripts\activate.bat
```

On Unix or MacOS:
```shell
source myproject-env/bin/activate
```

Sample Output (the output may slightly vary depending on the OS):
```shell
(myproject-env) $
```

### Installing Packages
Use `pip`, the package installer for Python, to install, upgrade, and remove packages. Here is how you can install a popular third-party library, `requests`, to make HTTP requests:

```shell
pip install requests
```

Sample Output:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Setting Up a Project Structure
A typical Python project might look something like this:

```
myproject/
│
├── myproject-env/    # Virtual environment
├── docs/             # Documentation
├── tests/            # Unit and integration tests
│   └── __init__.py
├── myproject/        # Project source code 
│   ├── __init__.py
│   └── main.py
├── setup.py          # Project setup file
└── README.md         # Project overview
```

### Create Your First Program
Create a `main.py` file inside the `myproject` directory. Here is an example of a simple program:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```

Run your program:

```shell
python myproject/main.py
```

Sample Output:
```shell
Hello, World!
```

### Use a Framework for Larger Projects
For larger projects, especially web applications, frameworks like Django or Flask are invaluable. Here's how to install Flask and create a simple "Hello, World" web application:

```shell
pip install Flask
```

Create a `app.py` file with the following content:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Run the Flask application:

```shell
flask run
```

Sample Output:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Navigate to `http://127.0.0.1:5000/` in your web browser, and you should see the "Hello, World!" message.
