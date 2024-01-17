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

## What & Why?

CSV stands for Comma Separated Values, and it is a file format used for storing data in a tabular form. It is commonly used for exchanging data between different programs, as it is a simple and universal format that can be easily read by most programming languages. Programmers use CSV because it allows them to easily store, access and manipulate large amounts of data in a structured manner.

## How to:

To work with CSV in Ruby, we first need to require the "csv" library. This can be done by writing `require 'csv'` at the beginning of your code. 

### Reading from a CSV File
To read data from a CSV file, we can use the `CSV.foreach` method. This method takes two arguments - the file path and a hash of options. Here's an example:
```Ruby
CSV.foreach('data.csv', headers: true) do |row|
  puts row['Name'] #assuming the first row in the CSV file contains a column called "Name"
end
```
The `headers: true` option specifies that the first row in the CSV file contains column headers. We can then access the data in each row using the column headers as keys.

### Writing to a CSV File
To write data to a CSV file, we can use the `CSV.open` method. This method takes two arguments - the file path and a hash of options. Here's an example:
```Ruby
CSV.open('new_data.csv', 'w', headers: true) do |csv|
  csv << ['John', 'Doe', 25] #writing data as an array
  csv << { name: 'Jane', surname: 'Smith', age: 30 } #writing data as a hash
end
```
The `'w'` mode specifies that the file should be opened for writing. Again, the `headers: true` option is used to specify that the first row should contain column headers. We can then write data to the file by using the `<<` operator.

## Deep Dive

CSV was first introduced in the early 1970s as a way to store data in a structured format. It gained popularity in the 1990s and has since become a widely used file format due to its simplicity and compatibility with different programs. Alternatives to CSV include XML and JSON, which are more versatile but can be more complex and less human-readable.

When working with CSV in Ruby, it is important to understand how different versions of Ruby handle encoding. In older versions, CSV data was assumed to be in the same encoding as the current locale. However, in newer versions, CSV data is assumed to be in UTF-8 encoding unless specified otherwise. This can lead to unexpected results when working with non-English characters.

## See Also

To learn more about working with CSV in Ruby, you can refer to the official Ruby documentation on CSV (https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html). You can also check out other sources or tutorials on how to use CSV in Ruby. Happy coding!