---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Att jobba med CSV (Comma-Separated Values) innebär att hantera data i en enkel, textbaserad filformat som är lätt att använda för att importera och exportera data från olika program. Programmerare använder CSV för att enkelt flytta data mellan system och för att göra det lättläst för människor.

## How to:
För att jobba med CSV-filer i PHP, kan du använda inbyggda funktioner som `fgetcsv` och `fputcsv`. Se exempel nedan:

```PHP
// Läsa från en CSV-fil
if (($handle = fopen("exempel.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($handle);
}

// Skriva till en CSV-fil
$list = array(array('ID', 'Namn', 'E-post'), array(1, 'Anna Svensson', 'anna@example.com'));

$fp = fopen('nyfil.csv', 'w');
foreach ($list as $fields) {
    fputcsv($fp, $fields);
}
fclose($fp);
```

Sample output för läsning av CSV:
```
Array
(
    [0] => ID
    [1] => Namn
    [2] => E-post
)
Array
(
    [0] => 1
    [1] => Anna Svensson
    [2] => anna@example.com
)
```

## Deep Dive
CSV-filformatet finns i många år och stödjs av de flesta databas- och kalkylprogram. Det finns alternativ som JSON och XML, men CSV hålls populärt för sin enkelhet. I PHP, är hanteringen av CSV-filer robust och möjliggör enkla anpassningar, som att ändra skiljetecken eller encapsulation character.

## See Also
- PHP Manual on fgetcsv: https://www.php.net/manual/en/function.fgetcsv.php
- PHP Manual on fputcsv: https://www.php.net/manual/en/function.fputcsv.php
- W3Schools introduction to CSV: https://www.w3schools.com/php/php_file_open.asp
