---
title:                "Att skriva en textfil"
html_title:           "Python: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skriva en textfil är en viktig del av programmering då det ger möjlighet att spara data permanent på en dator. Det kan vara användbart för att behålla information som genereras från programmet eller för att skapa en "log"-fil som visar vad som har gjorts under körning.

## Såhär gör du:
Divertimento
Det enklaste sättet att skriva en textfil i Python är att använda inbyggda funktionen ```open()``` för att öppna eller skapa en ny fil. Här är ett exempel på kod som lägger till texten "Hej världen!" i en textfil som heter "mitt_program.txt":

```python
f = open("mitt_program.txt", "w") f.write("Hej världen!") f.close() ```

Efter att koden har körts, kan du öppna filen "mitt_program.txt" och se texten "Hej världen!" skriven i den.

## Djupdykning:
I äldre versioner av Python behövde man använda ```write()``` funktionen för att skriva i en textfil. Denna funktion ersattes senare av ```f.write()``` för att bättre följa programmeringskonventioner. Om du vill lägga till mer text i samma fil kan du öppna den igen med ```f = open("mitt_program.txt", "a")``` innan du använder ```f.write()``` igen.

En annan väg att skapa en textfil är att använda modulen "io" och ```with```-satsen. Denna metod stänger automatiskt filen efter att den har använts.

```python
import io with io.open("mitt_program.txt", "w") as f: f.write("Hej världen!") ```

## Se även:
Här är några användbara länkar för att lära dig mer om skrivning av textfiler i Python:

- Dokumentation för inbyggda funktionen ```open()```: https://docs.python.org/sv/latest/library/functions.html#open
- Hur man läser från och skriver till filer i Python: https://realpython.com/read-write-files-python/
- "io" modulens dokumentation: https://docs.python.org/sv/latest/library/io.html