---
title:                "Python: Läser en textfil"
simple_title:         "Läser en textfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en vanlig uppgift inom programmering. Det kan hjälpa till att hantera stora mängder data eller att läsa in viktig information från externa källor.

## Hur man gör

För att läsa en textfil i Python använder vi inbyggda funktionen `open()` som tar emot två argument - filnamn och läsmodus. Sedan kan vi använda metoder som `.read()` eller `.readlines()` för att läsa filens innehåll.

```Python
# Öppna filen för läsning
file = open("exempelfil.txt", "r")

# Läs in hela filen
file_content = file.read()
print(file_content)

# Läs in rad för rad
file = open("exempelfil.txt", "r")
for line in file:
  print(line)

# Stäng filen
file.close()

#Exempelfil.txt innehåller:
#Detta är en exempeltext
#som vi vill läsa in i Python.
```

Output:
```
Detta är en exempeltext
som vi vill läsa in i Python.
```

## Djupdykning

Det finns flera olika sätt att hantera filer i Python, till exempel genom att läsa, skriva, ändra eller skapa nya filer. För att läsa filer på ett effektivt sätt kan vi använda `with`-satsen för att hantera öppnandet och stängandet av filen automatiskt.

```Python
# Öppna och läs in filen med with-satsen
with open("exempelfil.txt", "r") as file:
  for line in file:
    print(line)
```

För att läsa filer i olika format, till exempel CSV- eller JSON-filer, kan vi använda inbyggda moduler som `csv` eller `json` för att underlätta hanteringen av data.

För mer komplexa textbehandlingar finns också möjligheten att använda reguljära uttryck. Detta kan vara användbart för att söka och ersätta innehåll i filer eller för att filtrera ut specifika delar av texten.

## Se även

- [Python dokumentation för textfiler](https://docs.python.org/sv/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial: Läsning och skrivning av textfiler i Python](https://www.digitalocean.com/community/tutorials/how-to-handle-plain-text-files-in-python-3)
- [Multi-line strings i Python](https://www.pythonforbeginners.com/basics/multi-line-comments-using-parentheses-brackets-and-blackslash)