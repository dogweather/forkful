---
title:                "Working with csv"
html_title:           "Arduino recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Why 

CSV (Comma Separated Values) files are commonly used for storing and exchanging data in a structured format. Engaging in working with CSV on Arduino boards can be beneficial for projects that involve data collection, storage, and analysis. 

## How To 

To work with CSV files on Arduino, you will need to use a library called "CSV Reader." First, make sure you have the latest version of Arduino IDE installed. Then, follow these steps: 

- Open the Arduino IDE and create a new sketch. 
- Add the "CSV Reader" library to your project using the Library Manager. This can be found under "Tools" in the menu bar. 
- Include the library at the beginning of your sketch by adding `#include <csv.h>` 
- Next, you will need to initialize the CSV reader by creating an instance of the `CSV` object. This can be done by adding the following code: 
```Arduino 
CSV csv; 
```
- Now, you can start reading and writing data from a CSV file. For example, if you want to read data from a file called "data.csv," you can use the following code: 
```Arduino 
if(csv.init("data.csv")) { 
    // Read data here 
} 
```
- To read data, you can use the `read()` function which takes in the row and column number as parameters. For example, to read the data in the first row and second column, you can use `csv.read(1,2)`. 
- Similarly, to write data to a CSV file, you can use the `write()` function which takes in the row, column number, and the data to be written as parameters. For example, `csv.write(1,2, "Hello")` will write "Hello" in the first row and second column. 
- Finally, don't forget to close the CSV file using `csv.close()` when you are finished working with it. 

### Sample Output 

To give you a better understanding, let's take a look at a simple example. In this example, we will be reading data from a CSV file and displaying it on the serial monitor. 

```Arduino 
#include <csv.h> 

CSV csv; 

void setup() { 
    Serial.begin(9600); 
} 

void loop() { 
    if(csv.init("data.csv")) { 
        //Read data from first row and second column 
        String data = csv.read(1,2); 
        Serial.println(data); 
    } 

    csv.close(); //Close the CSV file 
    delay(1000); 
} 
```

Assuming our "data.csv" file contains the following data: 

|   | A  | B  | 
|---|----|----| 
| 1 | 10 | 20 | 

The output on the serial monitor will be: 

```
20 
``` 

## Deep Dive 

The "CSV Reader" library also provides some advanced features such as skipping rows, setting a custom delimiter, and reading multiple files at once. You can explore these features by referring to the library's documentation. 

It is worth noting that the `CSV` object also has a `totalRows()` function which returns the total number of rows in the file, and a `totalColumns()` function which returns the total number of columns in the file. These functions can be useful when working with large CSV files. 

## See Also 

To learn more about using the "CSV Reader" library, check out the official documentation and examples: 
- [CSV Reader Documentation](https://arduiniana.org/libraries/csv/) 
- [CSV Reader Examples](https://github.com/bertrandom/sd2card/tree/master/examples)