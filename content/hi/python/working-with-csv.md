---
title:                "CSV के साथ काम करना"
html_title:           "Python: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Kyun

CSV ka upyog karna bahut aasan hai aur isse hum apne data ko organize aur manipulate kar sakte hai. Python mein CSV ka upyog bahut common hai aur isse hum data analysis aur data processing mein kaam kar sakte hai.

## Kaise Kare

Sabse pehle humein CSV file ko read karne ke liye `csv` library ko import karna hoga:

```Python
import csv
```

CSV file ko read karne ke liye hum `open()` function ka use karenge. Is function mein humein file ka naam aur mode specify karna hai. Mode mein humein 'r' (read) mode use karna hai:

```Python
with open('file.csv', 'r') as csv_file:
  # code to read CSV file
```

CSV file mein rows ko access karne ke liye hum `csv.reader()` ka use karenge aur humein `for` loop ka use karna hoga:

```Python
with open('file.csv', 'r') as csv_file:
  csv_reader = csv.reader(csv_file)
  for row in csv_reader:
    # code to access rows
```

Agar humein sirf specific columns ki values chahiye toh hum `csv.DictReader()` ka use kar sakte hai:

```Python
with open('file.csv', 'r') as csv_file:
  csv_reader = csv.DictReader(csv_file)
  for row in csv_reader:
    column_value = row['column_name']
```

CSV file mein new data add karne ke liye hum `csv.writer()` ka use karenge aur usmein humein file ka naam aur mode specify karna hoga:

```Python
with open('file.csv', 'a') as csv_file:
  csv_writer = csv.writer(csv_file)
  # code to add new data
```

## Gehri Jhanki

CSV ke format mein data commas ke sath separate hota hai aur ismein text, numbers, aur dates save kiye jaate hai. Ismein data tabular format mein hota hai jismein rows aur columns hote hai. Hum `csv` library ke functions ka use karke CSV file ko read aur modify kar sakte hai.

## Dekhiye Bhi

- [Python CSV documentation](https://docs.python.org/3/library/csv.html)
- [Working with CSV in Python](https://www.geeksforgeeks.org/working-csv-files-python/)
- [Manipulating CSV files using pandas](https://www.datacamp.com/community/tutorials/pandas-read-csv)