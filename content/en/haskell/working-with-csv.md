---
title:                "Working with csv"
html_title:           "Haskell recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma Separated Values) files is a common task for many programmers. CSV files are a popular format for storing data in a structured way, and therefore, it is often necessary to read and write information from these files in a program. CSV files can hold large amounts of data while remaining human-readable, making them a versatile choice for data storage and manipulation.

## How to:
Reading and writing CSV files in Haskell is made simple with the help of the "csv" library. First, you will need to import it into your program:

```Haskell
import Text.CSV
```

Next, let's look at an example of reading data from a CSV file, assuming it has three columns: "Name", "Age", and "Occupation".

```Haskell
main = do
    -- Load the CSV file using the provided "parseCSVFromFile" function
    file <- parseCSVFromFile "data.csv"

    case file of
        -- If the file was successfully loaded, process the data
        Right csv -> do
            -- Remove the header (first row) from the CSV file
            let csv' = tail csv
            -- Extract a list of rows, where each row is represented as a list of strings
            let rows = map snd csv'
            -- Convert the string rows to lists of strings, representing each value
            let values = map (\x -> [head x, head $ tail x, head $ tail $ tail x]) rows
            -- Print the values to the console
            print values
        -- If there was an error loading the file, print the error message
        Left error -> putStrLn $ "Error: " ++ error
```

Given the following data in "data.csv":

```
Name,Age,Occupation
John,25,Software Engineer
Jane,30,Data Scientist
```

The output of the code above would be:

```
[["John", "25", "Software Engineer"], ["Jane", "30", "Data Scientist"]]
```

Writing data to a CSV file follows a similar process. Here is an example of writing the same data back to a new CSV file:

```Haskell
import Text.CSV

main = do
    -- Create the data to write to the CSV file
    let dataToWrite = [["Name", "Age", "Occupation"], ["John", "25", "Software Engineer"], ["Jane", "30", "Data Scientist"]]
    -- Write the data to a new CSV file, with the given filename
    writeCSV "new_data.csv" dataToWrite
```

The result is a new CSV file with the following contents:

```
Name,Age,Occupation
John,25,Software Engineer
Jane,30,Data Scientist
```

## Deep Dive:
CSV files have been around since the early days of computers, dating back to the 1970s. They were originally used as a way to transfer data between mainframe computers and personal computers. Over the years, they have become a popular format for storing and sharing data due to their simplicity and compatibility with different programming languages and applications.

While the "csv" library is the standard way for working with CSV files in Haskell, there are other alternatives such as the "cassava" library which provides more advanced features such as type-safe parsing and encoding.

The "Text.CSV" module of the "csv" library provides functions for parsing and writing CSV files, but it also includes features for manipulating CSV data, such as merging and sorting rows, and handling different separator characters (not just commas).

## See Also:
- [The csv package on Hackage](https://hackage.haskell.org/package/csv)
- [The cassava package on Hackage](https://hackage.haskell.org/package/cassava)
- [The CSV file format on Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)