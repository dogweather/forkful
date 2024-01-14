---
title:                "Bash recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

If you're unfamiliar with YAML, it may seem like just another confusing programming language to learn. But taking the time to familiarize yourself with YAML can actually greatly improve your Bash scripting skills. YAML is a lightweight markup language that is commonly used for configuring applications and storing data in a human-readable format. By learning how to work with YAML, you'll have a valuable tool in your programming arsenal.

## How To

### Installing and Using YAML in Bash

To begin, we first need to install the YAML library for Bash. This can be done using the following command:

```Bash
sudo apt-get install libyaml-dev
```

Once the library is installed, we can start working with YAML in our Bash scripts. To do this, we'll use the `yaml` command, which is part of the `libyaml-dev` package. Below is an example of how we can use this command to create a YAML file named `data.yml` containing some basic key-value pairs:

```Bash
yaml set name "John Doe" data.yml
yaml set age 30 data.yml
```

This will create a file with the following content:

```Bash
name: "John Doe"
age: 30
```

As you can see, YAML uses a simple key-value syntax that makes it easy to read and work with.

### Parsing YAML in Bash

Now that we have a YAML file, let's see how we can parse it in our Bash script. We can use the `yaml` command once again, this time with the `get` option to retrieve values from the YAML file. For example, if we want to get the value for the `name` key, we can use the following command:

```Bash
name=$(yaml get name data.yml)
echo "Hello, my name is $name."
```

This will output: `Hello, my name is John Doe.`

### Looping through YAML in Bash

One of the great things about YAML is its ability to store complex data structures. We can even store lists in YAML, making it a perfect fit for looping through data in Bash. Let's take a look at an example of how this might work. Say we have a YAML file with the following content:

```Bash
fruits:
  - apple
  - banana
  - orange
```

We can use the `yaml get` command with the `--keys` option to retrieve a list of keys from our YAML file. Then, we can use a `for` loop to iterate through each item in the `fruits` list:

```Bash
# Get list of keys
keys=$(yaml get fruits data.yml --keys)
# Loop through each item in list
for item in $keys
do
  echo "I like $item."
done
```

This will output:

```Bash
I like apple.
I like banana.
I like orange.
```

## Deep Dive

Now that we've covered the basics of working with YAML in Bash, let's take a deeper dive into some of its more advanced features.

### Passing YAML as a Variable in Bash

Instead of creating a separate YAML file, we can also pass YAML as a variable directly in our Bash script. This can be done using the `-y` option with the `yaml set` command. For example, we can create a variable named `animals` containing a YAML list:

```Bash
animals='- cat
- dog
- rabbit'
```

As you can see, we can use a simple indentation to maintain the YAML structure. Then, we can pass this variable to the `yaml set` command to create a YAML file named `pets.yml`:

```Bash
yaml set pets "$animals" pets.yml
```

This will create a YAML file with the following content:

```Bash
pets:
  - cat
  - dog
  - rabbit
```

### Manipulating YAML with Bash Arrays

Another useful feature of YAML is its compatibility with Bash arrays. We can use arrays in our Bash script to easily manipulate and work with YAML data. Let's take a look at an example. Say we have a YAML file with the following content:

```Bash
numbers:
  - 1
  - 2
  - 3
```

We can use the `yaml get` command with the `--values` option to retrieve a list of values from our YAML file. Then, we can use the `+=` operator to add each value to our Bash array:

```Bash
# Get list of values
values=$(yaml get numbers data.yml --values)
# Create Bash array
arr=()
# Loop through each value and add to array
for value in $values
do
  arr+=($value)
done
```

Now, we can easily perform array operations on our YAML data within our Bash