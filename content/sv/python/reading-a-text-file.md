---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil består i att extrahera data från ett textdokument för användning i den egna koden. Programmerare behöver göra detta för att få tillgång till och analysera data, som kan säkerställa att deras program fungerar som tänkt.

## Hur gör man:
För att läsa en textfil i Python, använder vi den inbyggda `open()`-metoden. Här är ett exempel:

```Python
# Öppna filen i 'read' läge
with open('sample.txt', 'r') as file:
    # Läs innehållet
    content = file.read()
# Skriv ut filens innehåll
print(content)
```

När du kör ovanstående kod, kommer outputen att visa innehållet i `sample.txt`.

## Närmare titt
Filhantering är en av de äldsta funktionerna i programmering, och går tillbaka till de tidiga dagarna av datorer när data lagrades på stora magnetiska band. Alternativ till Python för att läsa textfiler inkluderar andra programmeringsspråk som Java, C++ och Ruby. 

När man läser en textfil i Python, skapas ett filobjekt. Detta objekt har metoder som kan användas för att manipulera innehållet i filen, till exempel `read()`, `readline()`, eller `readlines()`. Men det är viktigt att komma ihåg att alltid stänga filen när du är klaar, antingen manuellt med `close()` metoden, eller genom att använda `with`-satsen, vilket automatiskt stänger filen när operationen är klara.

## Se också
För ytterligare information och grundläggande översikter över filhantering i Python, besök följande länkar:
- [Officiell Python-dokumentation om filhantering](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python för nybörjare - Filhantering](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)
- [W3Schools Python File Handling Tutorial](https://www.w3schools.com/python/python_file_handling.asp)