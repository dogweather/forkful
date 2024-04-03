---
date: 2024-03-08 21:33:21.816762-07:00
description: 'How to: Dart uses the `dart:io` library to work with files and directories.
  Here is a simple way to check if a directory exists.'
lastmod: '2024-03-13T22:44:59.833602-06:00'
model: gpt-4-0125-preview
summary: Dart uses the `dart:io` library to work with files and directories.
title: Checking if a directory exists
weight: 20
---

## How to:
Dart uses the `dart:io` library to work with files and directories. Here is a simple way to check if a directory exists:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('Directory exists');
  } else {
    print('Directory does not exist');
  }
}
```
Sample output if the directory does exist:
```
Directory exists
```

Or, if it does not:
```
Directory does not exist
```

To handle more complex scenarios, such as asynchronous checking or creating a directory if it doesn't exist, you could use the following approach:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // Asynchronously check if the directory exists
  var exists = await directory.exists();
  if (exists) {
    print('Directory exists');
  } else {
    print('Directory does not exist, creating...');
    await directory.create(); // This creates the directory
    print('Directory created');
  }
}
```

Sample output if the directory did not exist and was created:
```
Directory does not exist, creating...
Directory created
```

Dart's built-in capabilities are usually sufficient for handling files and directories, so third-party libraries are typically not necessary for this task. However, for more complex file system operations, packages like `path` (for manipulating paths in a platform-agnostic way) can complement the `dart:io` library but do not directly offer more advanced directory existence checks than what is shown.
