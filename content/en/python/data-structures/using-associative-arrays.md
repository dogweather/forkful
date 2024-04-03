---
date: 2024-01-30 18:57:25.397240-07:00
description: 'How to: Creating a dictionary in Python is straightforward. You enclose
  key-value pairs in curly braces `{}`, with keys and values separated by a colon.'
lastmod: '2024-03-13T22:44:59.701807-06:00'
model: gpt-4-0125-preview
summary: Creating a dictionary in Python is straightforward.
title: Using associative arrays
weight: 15
---

## How to:
Creating a dictionary in Python is straightforward. You enclose key-value pairs in curly braces `{}`, with keys and values separated by a colon:

```Python
# Create an associative array (dictionary)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Output:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Accessing a value by its key is simple:

```Python
# Access a value
print(my_dict["name"])
```

Output:
```
John
```

Adding or updating elements is done by assigning a value to a key:

```Python
# Add a new key-value pair
my_dict["email"] = "john@example.com"
# Update a value
my_dict["age"] = 31
print(my_dict)
```

Output:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

To iterate over the dictionary items:

```Python
# Iterate through key-value pairs
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Output:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Deep Dive
Associative arrays in Python, or dictionaries, were introduced to provide a data structure for efficient data access and manipulation. Unlike sequences, which are indexed by a range of numbers, dictionaries are indexed by keys, which can be any immutable type. This design choice makes dictionaries ideally suited for fast lookup tables where keys map to unique values.

Historically, Python dictionaries have been implemented using a hash table, ensuring that the average time complexity for lookup, insert, and delete operations is O(1). In Python 3.6 and later, dictionaries also maintain the insertion order of items, combining the benefits of hash tables with the predictability of insertion order seen in ordered data structures.

While dictionaries are incredibly versatile, in some specialized cases, alternatives like `collections.defaultdict` or `collections.OrderedDict` (before Python 3.7) might be preferable. `defaultdict` is particularly useful when you need a dictionary to return a default value for nonexistent keys, simplifying certain types of conditional logic. However, with the continuous improvement and evolution of Python, the built-in dictionary class often remains the go-to choice for associative arrays due to its robustness and the convenience it offers right out of the box.
