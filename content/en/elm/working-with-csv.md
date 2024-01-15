---
title:                "Working with csv"
html_title:           "Elm recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

 CSV (Comma Separated Values) files are a common way of storing and exchanging data. Many applications use CSV as a standard format for importing and exporting data. By learning how to work with CSV in Elm, you can efficiently manipulate and analyze large datasets and integrate them into your applications.

## How To

To begin working with CSV in Elm, you will first need to install the elm-csv package. This can be done by running the following command in your project directory:

```Elm
elm install elm-csv
```

Once the package is installed, you can import it into your Elm file using the following code:

```Elm
import Csv
```

To read a CSV file, you can use the `Csv.Decode.decode` function. This function takes in a CSV file and converts it into a list of lists containing the data. For example, if we have a CSV file with the following data:

```csv
Name, Age, Occupation
John Doe, 30, Software Engineer
Jane Smith, 25, Designer
```

We can read the file in Elm using the following code:

```Elm
csvData : String
csvData =
  "Name, Age, Occupation\n" ++
  "John Doe, 30, Software Engineer\n" ++
  "Jane Smith, 25, Designer"

decodedData = Csv.Decode.decode csvData
```

The `decodedData` variable will now contain the following list structure:

```Elm
[ [ "Name", "Age", "Occupation" ]
, [ "John Doe", "30", "Software Engineer" ]
, [ "Jane Smith", "25", "Designer" ]
]
```

You can then use this list to manipulate and analyze the data as needed.

To write data to a CSV file, you can use the `Csv.Encode.encode` function. This function takes in a list of lists and converts it into a CSV file. For example, if we have the following data in Elm:

```Elm
data =
  [ [ "Name", "Age", "Occupation" ]
  , [ "John Doe", "30", "Software Engineer" ]
  , [ "Jane Smith", "25", "Designer" ]
  ]
```

We can convert it into a CSV file using the following code:

```Elm
csvFile = Csv.Encode.encode data
```

This will produce the following CSV file:

```csv
Name, Age, Occupation
John Doe, 30, Software Engineer
Jane Smith, 25, Designer
```

## Deep Dive

One important thing to note when working with CSV in Elm is that the data must be in a specific format. Each line of the file must end with a newline character (`\n`) and each field must be separated by a comma (`,`). It's also important to consider data types when working with CSV - for example, numbers may need to be converted to strings in order to be properly encoded.

There are also additional options available in the `Csv` package, such as the ability to customize the delimiter and handle empty fields. For more information, you can refer to the official documentation for the elm-csv package.

## See Also

- Official documentation for the elm-csv package: https://package.elm-lang.org/packages/elm/csv/latest/
- How to use Elm's CSV library: https://programmaticponderings.com/2020/08/28/using-elms-csv-library/
- Working with CSV files in Elm: https://levelup.gitconnected.com/working-with-csv-files-in-elm-ae8b33de5d35