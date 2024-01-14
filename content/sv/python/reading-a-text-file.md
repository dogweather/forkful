---
title:                "Python: Att läsa en textfil"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är en viktig del av att programmera i Python. Genom att läsa en textfil kan du importera data till dina program och behandla den på ett effektivt sätt.

## Så här gör du
För att läsa en textfil i Python, använd "open()" funktionen och ange filnamnet och läsmetoden som argument. Till exempel: 

```Python
file = open("exempel.txt", "r")
```

Om du vill läsa innehållet i filen som en sträng, använder du "read()" metoden: 

```Python
innehall = file.read()
```

Om du vill läsa innehållet rad för rad, kan du använda "readlines()" metoden: 

```Python
rader = file.readlines()
```

För att stänga filen, använd "close()" metoden: 

```Python
file.close()
```

Nedan är ett exempel på hur du kan läsa innehållet i en textfil och skriva ut det rad för rad: 

```Python
with open("exempel.txt", "r") as fil:
    for rad in fil:
        print(rad)
```

För att läsa och skriva till en fil samtidigt kan du använda "r+" eller "w+" som läsmetod. Se till att stänga filen efter att du har arbetat med den för att undvika problem med åtkomst av filen senare.

## Djupdykning
En viktig aspekt av att läsa en textfil är att vara medveten om filens kodning. Om du vet att din textfil är kodad på ett annat sätt än standard (UTF-8), måste du ange detta när du öppnar filen.

Till exempel, om din textfil är i Latin-1 kodning, måste du ange detta vid öppnandet av filen: 

```Python
file = open("exempel.txt", "r", encoding="latin-1")
```

Det är också viktigt att hantera fel när du läser en textfil. Om filen inte finns på den angivna sökvägen eller om du inte har tillräckliga rättigheter att läsa filen, kommer programmet att krascha. För att undvika detta bör du använda "try-except" block för felhantering.

## Se också
* [Python Dokumentation: File Input/Output](https://docs.python.org/3/tutorial/inputoutput.html)
* [Real Python: Reading and Writing Files in Python](https://realpython.com/read-write-files-python/)
* [GeeksforGeeks: Reading and Writing Files in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)