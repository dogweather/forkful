---
date: 2024-02-03 19:03:04.206562-07:00
description: "Working with CSV files in Ruby provides a straightforward approach to\
  \ handle tabular data. Programmers often engage in this practice for data parsing,\u2026"
lastmod: 2024-02-19 22:05:19.032473
model: gpt-4-0125-preview
summary: "Working with CSV files in Ruby provides a straightforward approach to handle\
  \ tabular data. Programmers often engage in this practice for data parsing,\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?

Working with CSV files in Ruby provides a straightforward approach to handle tabular data. Programmers often engage in this practice for data parsing, extraction, transformation, and storage, making it a critical skill for tasks involving data manipulation or analysis.

## How to:

Ruby includes the CSV library by default, which simplifies reading from and writing to CSV files. Here’s how you can leverage this for common tasks:

### Reading a CSV file
To read from a CSV file, you first require the CSV library. Then, you can iterate over rows or read them into an array.

```ruby
require 'csv'

# Reading each row as an array
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# Output for each row might look like this: ["data1", "data2", "data3"]
```

### Writing to a CSV
Writing to a CSV file is also straightforward. You can append to an existing file or create a new file to write.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# This creates or overwrites 'output.csv' with the specified headers and values.
```

### Parsing a CSV string
Sometimes you need to parse CSV data directly from a string. Here’s how:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# Expected output:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### Using SmarterCSV
For more complex CSV tasks, the `SmarterCSV` gem can be a valuable tool. First, install the gem:

```shell
gem install smarter_csv
```

Then, you can use it to deal with large files or perform more sophisticated parsing and manipulation:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# This will read 'large_data.csv' and output each row as a hash based on the headers.
```

To sum up, Ruby's built-in CSV library, along with third-party gems like `SmarterCSV`, provides robust support for handling CSV data, allowing for efficient data processing and manipulation tasks.
