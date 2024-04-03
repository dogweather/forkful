---
date: 2024-01-25 03:40:14.382289-07:00
description: "TOML, short for Tom's Obvious, Minimal Language, is a data serialization\
  \ format akin to JSON or YAML, but aims for simplicity and readability. Programmers\u2026"
lastmod: '2024-03-13T22:44:59.728536-06:00'
model: gpt-4-1106-preview
summary: TOML, short for Tom's Obvious, Minimal Language, is a data serialization
  format akin to JSON or YAML, but aims for simplicity and readability.
title: Working with TOML
weight: 39
---

## What & Why?
TOML, short for Tom's Obvious, Minimal Language, is a data serialization format akin to JSON or YAML, but aims for simplicity and readability. Programmers use TOML for configuration files because it's easy to write and understand, and it maps neatly onto data structures in programming languages like Python.

## How to:
Before diving in, install the `toml` package with `pip install toml`. Let's parse a TOML file:

```python
import toml

# Example TOML content as a string
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # First class dates

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Parse the TOML string
parsed_toml = toml.loads(toml_string)

# Accessing data
print(parsed_toml['owner']['name'])  # Output: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Output: [8001, 8001, 8002]
```

## Deep Dive
TOML was created by Tom Preston-Werner, one of the founders of GitHub, as a more user-friendly configuration file format. It's designed to unambiguously map to a hash table and be easily parsable by machines.

Compared to JSON, TOML is more readable for config files and supports comments. YAML, another alternative, can be more compact, but its reliance on indentation and subtle issues, like how tabs aren't allowed, can trip people up. 

As for implementation details, TOML values are typed, which includes strings, integers, floats, booleans, datetimes, arrays, and tables. Everything is case-sensitive. Also, TOML supports multi-line strings and, as of the latest version, even allows for heterogeneously typed arrays.

Python uses the `toml` library, which mirrors the JSON and YAML libraries in terms of API. You have `toml.load` and `toml.loads` for reading TOML from a file or a string, respectively, and `toml.dump` and `toml.dumps` for writing it out.

## See Also
- The official TOML GitHub repository for specs: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- The `toml` Python library documentation: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Real-world examples of TOML: Config files for Rust's package manager `cargo` or the Python packaging tool `poetry`.
