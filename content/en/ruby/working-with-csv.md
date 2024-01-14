---
title:                "Ruby recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) files are a popular file format for storing tabular data. These files can be easily opened in various software such as Microsoft Excel or Google Sheets. Ruby offers a built-in library for working with CSV files, making it a convenient choice for managing large datasets. 

## How To

Working with CSV files in Ruby is a simple and straightforward process. First, we need to require the CSV library in our Ruby file.

```
require 'csv'
```

Next, we can use the `foreach` method to read and iterate through a CSV file. This method takes in the file name as the first argument and an options hash as the second argument. The options hash allows us to specify things like the delimiter, encoding, and headers.

```
CSV.foreach("data.csv", headers: true) do |row|
  puts row["name"] # accessing data in the "name" column
end
```

We can also create a CSV file by using the `open` method. This method takes in the file name as the first argument and an array of arrays as the second argument.

```
CSV.open("new_data.csv", "w") do |csv|
  csv << ["name", "age", "occupation"]
  csv << ["John", 25, "Developer"]
  csv << ["Emma", 30, "Designer"]
end
```

And finally, we can update an existing CSV file by using the `open` and `<<` methods.

```
CSV.open("data.csv", "a+") do |csv|
  csv << ["Samantha", 40, "Manager"]
end
```

## Deep Dive

Ruby's CSV library also offers other useful methods for managing CSV files. For example, the `read` method allows us to read the entire CSV file and return it as an array of arrays.

```
csv_data = CSV.read("data.csv")
puts csv_data # [["name", "age", "occupation"], ["John", 25, "Developer"], ["Emma", 30, "Designer"]]
```

We can also use the `parse` method to convert a CSV string into an array of arrays.

```
csv_string = "name, age, occupation\nSamantha, 40, Manager\nPeter, 35, Engineer"
csv_data = CSV.parse(csv_string)
puts csv_data # [["name", "age", "occupation"], ["Samantha", 40, "Manager"], ["Peter", 35, "Engineer"]]
```

And for more complex CSV files, we can use the `table` or `foreach_row` methods to read the data in a structured format.

```
table = CSV.table("data.csv")
puts table # prints the CSV data in a table format with column headers and rows

CSV.foreach_row("data.csv", headers: true) do |row|
  puts row[:name] # accessing data using column header symbols
end
```

## See Also

- Ruby's official CSV library documentation: https://ruby-doc.org/stdlib-2.7.2/libdoc/csv/rdoc/CSV.html
- An introduction to working with CSV files in Ruby: https://www.rubyguides.com/2018/10/parse-csv-ruby/
- A tutorial on creating and manipulating CSV files in Ruby: https://www.digitalocean.com/community/tutorials/working-with-csv-data-in-ruby