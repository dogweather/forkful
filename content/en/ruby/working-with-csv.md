---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) files represent tabular data in plain text. Programmers use CSVs to import and export datasets because they're widely supported, easy to read, and straightforward to parse.

## How to:

**Reading a CSV:**

```Ruby
require 'csv'

CSV.foreach("path/to/file.csv", headers: true) do |row|
  puts row["HeaderName"] # Replace with your actual header
end
```

**Writing to a CSV:**

```Ruby
require 'csv'

CSV.open("path/to/output.csv", "wb", write_headers: true, headers: ["Name", "Age", "City"]) do |csv|
  csv << ["Alice", 32, "Wonderland"]
  csv << ["Bob", 46, "Springfield"]
end
```

**Sample Output:**

```Text
Alice, 32, Wonderland
Bob, 46, Springfield
```

## Deep Dive

CSV has been around since the early days of computing, offering a simple way to move tabular data between programs and systems. Alternatives include JSON and XML, but CSV remains popular for its simplicity and low overhead. Ruby's standard CSV library, conveniently wrapped around the underlying parsers, offers seamless integration including support for different encodings, custom converters, and flexible parsing options.

## See Also

- Ruby's CSV library documentation: https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html
- CSV on Wikipedia: https://en.wikipedia.org/wiki/Comma-separated_values
- "FasterCSV" gem (old but relevant for historical reasons): https://rubygems.org/gems/fastercsv