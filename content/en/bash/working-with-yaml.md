---
title:                "Working with yaml"
html_title:           "Bash recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
So you've heard about YAML and you're wondering why you should bother learning it. Well, YAML is a simple and easily readable data serialization language, making it perfect for storing and sharing data in a variety of applications. Plus, it's widely used and supported, so it's definitely worth adding to your programming skill set.

## How To
Now that you know why YAML is worth learning, let's dive into some coding examples to get you started. We'll be using Bash, the current version, to demonstrate how to work with YAML.

First, you'll need to make sure you have YAML installed on your system. If you're using a Linux distribution, chances are it's already installed. If not, you can easily install it using the command ```sudo apt-get install libyaml-dev```.

Next, let's create a simple YAML file using the ```cat``` command. Just open your terminal and type:

```Bash
cat > sample.yaml
```

This will open up a blank file where you can start typing your YAML code. Let's add a simple key-value pair:

```Bash
name: John Doe
```

To save and exit the file, press CTRL+D. Now let's use the ```cat``` command again to print the contents of our YAML file:

```Bash
cat sample.yaml
```

You should see the following output:

```Bash
name: John Doe
```

Congratulations, you've just created your first YAML file! Of course, YAML can handle much more complex data structures, but this simple example gives you a good idea of its syntax and structure.

## Deep Dive
For a deeper understanding of YAML, there are a few key concepts to keep in mind. First, YAML utilizes indentation to define the structure of data. This means that proper indentation is crucial for a valid YAML file. It also uses key-value pairs to store data, with the key and value separated by a colon.

YAML also supports arrays and dictionaries, making it a versatile language for storing and organizing different types of data. It even allows you to reference values within your YAML file using the ampersand (&) and asterisk (*) symbols.

To learn more about YAML's syntax and features, check out the official YAML specification document here.

## See Also
For more information on working with YAML, check out these helpful resources:

- https://yaml.org/
- https://www.tutorialspoint.com/yaml/index.htm
- https://github.com/jasperes/bash-yaml