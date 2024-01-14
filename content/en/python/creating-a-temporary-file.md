---
title:    "Python recipe: Creating a temporary file"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files may seem like a trivial task in Python programming, but it can greatly improve the efficiency and organization of your code. Temporary files are meant to be used for storing data temporarily and are automatically deleted once the program has finished running. This not only saves disk space, but also helps avoid clutter and potential conflicts with existing files.

## How To

Creating a temporary file in Python is a straightforward process. First, we need to import the built-in `tempfile` module.

```python
import tempfile
```

Next, we can use the `tempfile.NamedTemporaryFile()` function to create a temporary file and assign it to a variable.

```python
temp_file = tempfile.NamedTemporaryFile()
```

By default, this will create a temporary file in the system's default temporary directory. We can also specify the directory where we want the temporary file to be created.

```python
temp_file = tempfile.NamedTemporaryFile(dir="path/to/directory")
```

We can also specify the prefix and suffix for the temporary file name, which can be useful for differentiating between different temporary files in our code.

```python
temp_file = tempfile.NamedTemporaryFile(prefix="temp_", suffix=".txt")
```

To write data to the temporary file, we can use the `write()` method.

```python
temp_file.write("This is a temporary file.")
```

We can then use the `read()` method to retrieve the contents of the file.

```python
temp_file.read()
```

After we have finished using the temporary file, we should close it using the `close()` method.

```python
temp_file.close()
```

If we want to delete the temporary file before the program ends, we can use the `delete` parameter when creating the file.

```python
temp_file = tempfile.NamedTemporaryFile(delete=False)
```

## Deep Dive

Behind the scenes, the `tempfile` module uses the underlying operating system's functionality to create and manage temporary files. This means that the temporary file created by the `NamedTemporaryFile()` function will have a unique file name and will be stored in a secure location. Additionally, the file will be automatically deleted once the program has completed, even in the event of a crash.

The `tempfile` module also provides other functions for creating temporary directories and named pipes. It also supports different modes for reading and writing to the temporary file, similar to the built-in `open()` function.

## See Also

- [Python official documentation on tempfile](https://docs.python.org/3/library/tempfile.html)
- [GeeksforGeeks tutorial on tempfile in Python](https://www.geeksforgeeks.org/temporary-files-python/)
- [RealPython article on tempfile](https://realpython.com/python-tempfile/)