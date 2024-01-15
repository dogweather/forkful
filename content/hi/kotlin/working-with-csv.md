---
title:                "csv के साथ काम करना"
html_title:           "Kotlin: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Kyun
CSV (Comma Separated Values) ek bahut hi prachalit format hai jo data ko store karne aur share karne ke liye use kiya jata hai. Iska use bahut saari applications aur systems mein kiya jata hai, jaise financial data, database management, aur spreadsheet software. CSV ek simple, lightweight, aur easily readable format hai, isliye bahut se developers CSV ko apni programming projects mein prefer karte hai.

## Kaise
CSV ka use Kotlin mein text based data store karne ke liye kiya jata hai. Iske liye hum `FileReader` aur `BufferedReader` ka use karenge. Yeh dono classes `kotlin.io` package mein available hai. Chaliye ek simple CSV file ko padhne aur uske data ko console par print karne ka example dekhte hai:

```Kotlin
import java.io.FileReader
import java.io.BufferedReader

// CSV file ka path
val path = "file.csv"

// FileReader aur BufferedReader ka initialization
val fileReader = FileReader(path)
val bufferedReader = BufferedReader(fileReader)

// Saari lines ko read karte hue print karna
bufferedReader.use { reader ->
    reader.lines().forEach { line ->
        println(line)
    }
}
```

Output:

```
Name, Age, Salary
John, 25, $50,000
Sara, 30, $60,000
Brandon, 32, $70,000
```

Is example mein humne pehle CSV file ka path define kiya phir usko `FileReader` aur `BufferedReader` se read kiya. Saari lines ko read karne ke baad humne use `forEach` loop ka use karke print kiya. Is tarah se hum CSV file ke data ko Kotlin mein read aur print kar sakte hai.

## Deep Dive
CSV files mein data comma separated format mein hota hai, iss liye unko padhna thoda tricky ho sakta hai. Agar CSV file mein quotation marks ya special characters hai toh data ko sahi se read karna aur alag columns mein split karna difficult ho sakta hai. Iske liye hum CSV parsing libraries ka use kar sakte hai, jaise ki OpenCSV aur SuperCSV.

CSV files ko read karne ke alawa, Kotlin mein hum CSV data ko modify aur manipulate bhi kar sakte hai. Hum `FileWriter` aur `BufferedWriter` ka use karke CSV files mein new data add kar sakte hai. CSV files bahut flexible hai aur inka use data exchange ke liye bahut helpful hai.

## See Also
- [Kotlin Documentation](https://kotlinlang.org/docs/)
- [OpenCSV Library](http://opencsv.sourceforge.net/)
- [SuperCSV Library](https://www.supercsv.org/)