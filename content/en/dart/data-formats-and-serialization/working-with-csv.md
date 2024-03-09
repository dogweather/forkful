---
title:                "Working with CSV"
date:                  2024-03-08T21:33:55.387738-07:00
model:                 gpt-4-0125-preview
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma Separated Values) files involves parsing and generating text files where each line holds values separated by commas. Programmers do this to enable data exchange between different applications or to facilitate data storage in a lightweight, human-readable format.

## How to:

To handle CSV files in Dart, you typically either manually process the text or use third-party libraries to simplify the task. Here, we'll look at both approaches.

### Manually Parsing CSV

If your needs are simple, you might opt to manually parse a CSV string. This can be achieved using Dart's core string manipulation functions:

```dart
void main() {
  // Sample CSV data
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Splitting the CSV data into lines
  List<String> lines = csvData.split('\n');
  
  // Parsing each line
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // Output the parsed data
  print(data);
}

// Sample output:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Using a Third-Party Library: `csv`

For more complex scenarios or to simplify your code, you can use a popular third-party library like `csv`. First, add it to your project by including `csv: ^5.0.0` (or the latest version) in your `pubspec.yaml` file under `dependencies`. Then use it as follows:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Use the CsvToListConverter to parse the CSV data
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // The first list item contains headers
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // Removing the header row before processing further
  listData.removeAt(0);
  
  // Convert to List<Map<String, dynamic>> for a more structured format
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Output the mapped data
  print(mappedData);
}

// Sample output:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Both methods demonstrate how to work with CSV data: the first manually, for learning purposes or when dealing with very simple CSV structures; the second, by leveraging a powerful library that simplifies parsing and can handle various complexities of CSV formatting.
