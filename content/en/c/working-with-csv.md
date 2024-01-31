---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means processing data structured as plain text where each line has fields divided by commas. Programmers use CSV because it's simple, widely supported, and integrates easily with spreadsheets and databases.

## How to:

Let's parse a CSV file with basic C code. We'll read a file, split each line into fields, and print them out.

```C
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char line[256];
    while (fgets(line, sizeof(line), fp)) {
        char *token = strtok(line, ",");
        while (token) {
            printf("%s\n", token);
            token = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```

Sample `data.csv`:
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Sample output:
```
name
age
city
Alice
30
New York
Bob
25
Los Angeles
```

## Deep Dive

CSV files have been used since the early days of personal computing because of their simplicity. Alternatives like JSON or XML carry more complexity but offer structured data representation. When it comes to implementing CSV parsing, care must be taken to handle edge cases such as fields containing commas or newlines, which should be enclosed in quotes as per the CSV standard (RFC 4180).

## See Also

- [RFC 4180](https://tools.ietf.org/html/rfc4180): The Common Format and MIME Type for Comma-Separated Values (CSV) Files.
- [libcsv](http://sourceforge.net/projects/libcsv/): A C library for CSV parsing.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/csv?tab=Votes): Community discussions and Q/A about CSV-related programming issues.
