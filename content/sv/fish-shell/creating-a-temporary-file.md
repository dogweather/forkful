---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & varför?
Skapa en temporär fil, tillfälligt lagring av data för endast aktuell session, vilket är grundläggande för programmering. Det ger en säker plats för program att göra ändringar utan att störa användaruppgifter.

## Så här gör du:
För att skapa en tillfällig fil i Fish-shell, använd 'mktemp'. Dessutom kan du skriva och läsa från filen som vanligt.

```Fish Shell
set tmpfile (mktemp)
echo "temp data" > $tmpfile
cat $tmpfile
```
När du kör koden ovan, kommer du att se "temp data" skrivas ut, vilket är innehållet i den tillfälliga filen.

## Fördjupning
Historiskt sett kommer mktemp från Unix-traditionen och har varit standard för tillfällig filskapning sedan 90-talet. Alternativ till mktemp inkluderar att manuellt skapa en fil med ett unikt namn, men detta kan vara icke-trådsäkert. Under huven skapar mktemp en fil med unikt namn i ett lämpligt temporärt katalog (ofta /tmp eller liknande), och returnerar filnamnet.

## Se också
För mer information, se Fish Shells officiella dokumentation på [den här länken](https://fishshell.com/docs/current/index.html), samt Unix's mktemp manual på [den här länken](https://man7.org/linux/man-pages/man3/mktemp.3.html).