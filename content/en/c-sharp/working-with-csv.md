---
title:                "C# recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

If you're new to programming, chances are you've come across the term "CSV" before. But what exactly is it, and why would you want to work with it? CSV stands for "Comma Separated Values" and is a file format used for organizing and storing tabular data. It's commonly used in various fields, such as finance, marketing, and data analysis. In this blog post, we will explore the world of CSV files and why you might want to work with them in your programming projects.

## How To

To start working with CSV files in C#, we first need to understand the basic structure of a CSV file. Essentially, a CSV file is a plain text file that contains rows and columns of data, separated by commas (hence the name). Each row represents a data entry and each column represents a specific attribute or data point. For example, a CSV file that contains a list of products and their prices might look like this:

```
Product Name, Price
T-shirt, $20
Jeans, $40
Sneakers, $50
```

Now let's see how we can read, write, and manipulate CSV files in C#.

### Reading a CSV File

To read a CSV file in C#, we can use the built-in `StreamReader` class. Here's an example:

```C#
using (StreamReader reader = new StreamReader("products.csv"))
{
     while (!reader.EndOfStream)
     {
          var line = reader.ReadLine();
          var values = line.Split(',');
          Console.WriteLine("Product Name: " + values[0] + " Price: " + values[1]);
     }
}
```

We first create a `StreamReader` object and pass in the name of our CSV file. Then, we use a `while` loop to loop through each line of the file until we reach the end. We then split each line by the comma and access the values using their corresponding indices. Finally, we can use this data however we want, in this case, we simply print it to the console.

### Writing to a CSV File

To write to a CSV file, we can use the `StreamWriter` class. Here's an example:

```C#
using (StreamWriter writer = new StreamWriter("new_products.csv"))
{
     writer.WriteLine("Product Name, Price");
     writer.WriteLine("Hoodie, $35");
     writer.WriteLine("Sunglasses, $25");
     writer.WriteLine("Hat, $12");
}
```

We first create a `StreamWriter` object and pass in the name of our new CSV file. Then, we use the `WriteLine` method to write each line of data to the file. It's important to note that when writing to a CSV file, we need to include the column headings ourselves.

### Manipulating CSV Data

One of the great things about working with CSV files in C# is that we have access to various libraries and packages that make data manipulation much easier. For example, we could use the popular "CsvHelper" package to easily read and write CSV files, as well as perform more advanced tasks such as mapping CSV data to objects.

## Deep Dive

While CSV files may seem simple, there are some important considerations to keep in mind when working with them. One of the key factors is data formatting - when using CSV files, we need to be extra careful with data formatting to ensure accurate results. For example, we need to make sure all values are properly quoted and escaped to avoid any issues with comma or new line characters. Additionally, it's important to handle errors and exceptions when reading and writing to CSV files, as well as properly closing the file after use.

## See Also

- [Reading and Writing CSV Files in C#](https://www.c-sharpcorner.com/article/reading-and-writing-csv-files-using-C-Sharp/)
- [Manipulating CSV Files in C# with CsvHelper](https://www.codeproject.com/Tips/1039364/Manipulate-CSV-data-with-Csharp)
- [Working with Tabular Data in C#](https://medium.com/free-code-camp/working-with-tabular-data-in-c-7ac7a2f27a8d)