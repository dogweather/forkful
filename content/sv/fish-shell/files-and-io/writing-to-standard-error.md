---
aliases:
- /sv/fish-shell/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:24.180941-07:00
description: "Att skriva till standardfel (stderr) i Fish Shell handlar om att rikta\
  \ felmeddelanden eller diagnostik separat fr\xE5n standardutdata (stdout). Programmerare\u2026"
lastmod: 2024-02-18 23:08:52.223501
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Fish Shell handlar om att rikta felmeddelanden\
  \ eller diagnostik separat fr\xE5n standardutdata (stdout). Programmerare\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (stderr) i Fish Shell handlar om att rikta felmeddelanden eller diagnostik separat från standardutdata (stdout). Programmerare gör detta för att säkerställa att felinformation kan identifieras, hanteras eller omdirigeras lätt, vilket underlättar smidigare felsökning och loggningsprocesser.

## Hur man gör:

I Fish Shell kan du skriva till stderr genom att omdirigera din utdata med `>&2`. Här är ett grundläggande exempel:

```fish
echo "Det här är ett felmeddelande" >&2
```

Detta kommando gör helt enkelt så att ett meddelande ekoas till stderr istället för stdout. Om du skulle skriva ett skript som ger ut både vanliga meddelanden och felmeddelanden, skulle du kunna göra något sådant här:

```fish
echo "Startar processen"
echo "Ett fel inträffade" >&2
echo "Processen slutförd"
```

Exempelutdata om du kör skriptet och omdirigerar stderr till en fil:

```
Startar processen
Processen slutförd
```

Felmeddelandet skulle inte visas i standardutdata men skulle hittas i filen du omdirigerade stderr till.

I scenarier som kräver mer sofistikerad felsökning eller loggning, kommer Fish inte med inbyggda bibliotek uttryckligen avsedda för detta. Dock kan du utnyttja externa verktyg eller skriva funktioner för att hjälpa till. Till exempel skulle skapandet av en enkel loggningsfunktion kunna se ut så här:

```fish
function log_error
    echo $argv >&2
end

log_error "Det här är ett avancerat felmeddelande"
```

Denna funktion `log_error` kommer att ta vilken sträng som helst du ger den och skriva den till stderr. Att använda funktioner som denna kan hjälpa till att hålla din felsökning ren och konsekvent genom dina skript.
