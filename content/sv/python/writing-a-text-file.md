---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil i Python innebär att man sparar textdata till en fil på datorn. Programmerare gör detta för att spara indata, loggar, konfigurationer eller för att utbyta data mellan olika program.

## Gör såhär:
```Python
# Öppna en fil för skrivning (skapar filen om den inte finns)
with open('exempel.txt', 'w') as fil:
    fil.write('Hej världen!\n')  # Skriver en rad till filen

# Läser filen för att bekräfta innehållet
with open('exempel.txt', 'r') as fil:
    innehall = fil.read()
print(innehall)
```
Output:
```
Hej världen!
```

## Deep Dive
Från den tidiga UNIX-eran har det att spara textfiler varit standard för konfiguration och loggfiler, på grund av enkelhet och transparens. Alternativ inkluderar binära filer eller databaser, vilka kan vara mer effektiva men mindre lättlästa för människor. Vid skrivning av filer sköter Python buffring och teckenkodning, men det går att manipulera dessa inställningar.

## Se Också
- Python dokumentation för filhantering: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Python 'io' modulens dokumentation: https://docs.python.org/3/library/io.html
