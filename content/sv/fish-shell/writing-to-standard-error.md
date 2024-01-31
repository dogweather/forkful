---
title:                "Skriva till standardfel"
date:                  2024-01-19
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standardfel (stderr) är att dirigera felmeddelanden och diagnostisk information till en separat kanal från vanlig utdata (stdout). Programmerare gör detta för att separera normala programresultat från felsökning och felrapportering, vilket underlättar systemadministration och loggning.

## Så här gör du:
```Fish Shell
echo "Det här är ett vanligt meddelande"  # Skriver till standardutdata
echo "Det här är ett felmeddelande" >&2  # Skriver till standardfel
```
Exempelutdata:
```
Det här är ett vanligt meddelande
Det här är ett felmeddelande
```

## Fördjupning
Historiskt sett utvecklades konceptet med att separera standardutdata och standardfel inom Unix för att ge ett flexibelt utdatahantering. Alternativ till att skriva direkt till stderr inkluderar loggfiler och diagnostiska verktyg. I Fish Shell hanterar man detta genom att omdirigera utdata via `>&` följt av filbeskrivaren för standardfel, som är `2`.

## Se även
- Fish Shell dokumentation för omdirigeringar: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Unix filsystemets struktur och filbeskrivare: [https://en.wikipedia.org/wiki/Unix_filesystem](https://en.wikipedia.org/wiki/Unix_filesystem)
- Guide om loggning och felsökning i Unix-baserade system: [https://tldp.org/LDP/sag/html/](https://tldp.org/LDP/sag/html/)
