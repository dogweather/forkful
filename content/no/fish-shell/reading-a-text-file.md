---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil er prosessen der programvaren henter innholdet i filen for videre bruk eller analyse. Programmerere gjør dette for å høste data, lese konfigurasjon, eller behandle brukerinnsats fra filer. 

## Hvordan:
Her er noen grunnleggende eksempler på hvordan du kan lese en tekstfil i Fish Shell: 

```Fish Shell 
set fil "myfile.txt"
cat $fil
```
I dette eksemplet vil `cat` kommandoen lese innholdet i filen som er definert i variabelen `fil`. Den vil deretter skrive ut innholdet til standard utskrift (som vanligvis er terminalen din).

```Fish Shell 
set linjer (cat $fil)
echo $linjer[1]
```
I det andre eksemplet bruker vi Fish Funksjoner til å lese filen inn i en liste, så vi kan behandle hver linje separat. Deretter skriver vi ut den første linjen i filen.

## Dykket Ned:
Historisk sett har Unix og dets derivater benyttet `cat`, `less`, `more`, `tail`, og andre kommandoer for å lese tekstfiler. Selv om disse fortsatt er mye brukt, gir skall som Fish flere funksjoner og metoder for å arbeide med filer på en mer programmatiske måte.

Alternativer inkluderer bruk av `read` kommando som leser en linje om gangen istedenfor å lese hele filen på en gang som `cat` gjør. 

I implementeringsdetaljer, `cat` kommandoen fungerer ved å åpne filen, lese en bit a gangen, og skrive hvert stykke til standard utskrift. I Fish, kan du ta denne utdataen og lagre den til en variabel, som du deretter kan behandle videre.

## Se Også:
- Fish Dokumentasjon: https://fishshell.com/docs/current/ 
- Unix og Linux System Administrasjon Håndbok: https://www.wiley.com/en-us/UNIX+and+Linux+System+Administration+Handbook%2C+5th+Edition-p-9780134277554 
- Introduksjon til Fish: https://fishshell.com/docs/current/tutorial.html