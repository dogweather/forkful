---
title:                "Working with csv"
html_title:           "Ruby recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV (Comma Separated Values) files are a common data format used for storing and exchanging tabular data. Working with CSV files can be useful for tasks such as data analysis, importing/exporting data from databases, and creating reports. In order to work with CSV files, having a good understanding of how to manipulate and parse them using programming languages like Ruby can save time and improve efficiency.

## How To
To begin working with CSV files in Ruby, we first need to require the CSV library. This can easily be done by adding `require 'csv'` at the top of your file. Now, let's take a look at how we can read, write, and manipulate CSV files using Ruby.

#### Reading CSV Files
To read a CSV file, we can use the `CSV.foreach` method. This method takes two parameters - the path to the CSV file and an options hash. Here's an example:

```
CSV.foreach("data.csv", headers: true) do |row|
  puts row
end
```

This will print each row of the CSV file, with the first row being used as headers for the data. If we want to access specific columns of the data, we can do so by using the headers as keys. For example, `row['Name']` will give us the value in the "Name" column.

#### Writing CSV Files
To write data to a new CSV file, we can use the `CSV.open` method. This method takes two parameters - the path to the CSV file and an options hash. Here's an example:

```
data = [['Name', 'Age'], ['John', '25'], ['Jane', '30']]
CSV.open("new_data.csv", "w") do |csv|
  data.each do |row|
    csv << row
  end
end
```

This will create a new CSV file with the data provided. We can also use the `<<` operator to add new rows to an existing CSV file.

#### Manipulating CSV Data
The `CSV` library also provides useful methods for manipulating CSV data. For example, we can sort the data by a specific column, add or remove columns, and filter the data based on certain conditions. Here's an example of sorting CSV data by the "Age" column:

```
sorted_data = CSV.read("data.csv", headers: true).sort_by { |row| row['Age'] }
```

## Deep Dive
The `CSV` library also allows us to customize how we read and write CSV files by using different options in the `foreach` and `open` methods. We can also specify different delimiters and quote characters for our CSV files.

In addition, the `CSV` library automatically handles data formatting, such as converting numbers and dates to their appropriate types. It also has methods for converting CSV data to other data structures, such as hashes or arrays.

Lastly, it's important to keep in mind that the `CSV` library may not be the most efficient option for working with large datasets. In such cases, it may be better to use a specialized library or tool for handling big data.

## See Also
- [Ruby CSV documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [Working with CSV Files in Ruby](https://www.rubyguides.com/2018/10/working-with-csv-files-ruby/)
- [Manipulating CSV Data in Ruby](https://medium.com/free-code-camp/processing-csv-files-in-ruby-a-really-basic-cheat-sheet-1bbf7ea014f1)