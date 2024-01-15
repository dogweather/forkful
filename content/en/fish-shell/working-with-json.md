---
title:                "Working with json"
html_title:           "Fish Shell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON (JavaScript Object Notation) has become the de facto standard for exchanging data between applications. Working with JSON in Fish Shell can be a useful skill for automation, scripting, and managing data in various forms.

## How To
To work with JSON in Fish Shell, you will need a few tools. First, ensure you have installed the latest version of Fish Shell. JSON support is included in Fish Shell version 3.1. Next, you will need the `jq` command-line tool, which allows you to manipulate and query JSON data. Install it using your preferred package manager.

To begin working with JSON, let's start with a simple example. Let's say we have a JSON file called `books.json` that contains the following data:

```
[
    {
        "title": "To Kill a Mockingbird",
        "author": "Harper Lee",
        "publication_year": 1960
    },
    {
        "title": "1984",
        "author": "George Orwell",
        "publication_year": 1949
    },
    {
        "title": "Pride and Prejudice",
        "author": "Jane Austen",
        "publication_year": 1813
    }
]
```

We can use `jq` to extract specific information from this JSON file. For example, let's say we want to get a list of all the authors. We can do this by using the `map` function and specifying the `author` field:

```
➜ jq -r '. | map(.author)[]' books.json 
Harper Lee
George Orwell
Jane Austen
```

To further refine our query, we can use `select` to only retrieve authors whose books were published before 1900:

```
➜ jq -r '. | map(select(.publication_year < 1900) | .author)[]' books.json 
Jane Austen
```

## Deep Dive
If you want to work with more complex JSON data, `jq` has a wide range of powerful features to help you do so. For example, you can use `group_by` to group data by a specific field, or `sort_by` to sort data in a specific order. Check out the `jq` documentation for more information on all the available functions.

Additionally, Fish Shell has its own built-in `string` and `json` functions that can be useful for working with JSON data. The `string` function allows you to convert a string into JSON, while the `json` function can be used to parse JSON data. You can find more information about these functions in the Fish Shell documentation.

See Also
- The official Fish Shell website: https://fishshell.com/
- Official `jq` documentation: https://stedolan.github.io/jq/
- Fish Shell documentation on string and JSON functions: https://fishshell.com/docs/current/commands.html#string-functions and https://fishshell.com/docs/current/commands.html#json-functions