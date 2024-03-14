---
date: 2024-01-20 17:58:05.626221-07:00
description: "Att s\xF6ka och ers\xE4tta text \xE4r grundprocessen att hitta specifika\
  \ str\xE4ngar i data och byta ut dem mot andra str\xE4ngar. Programmerare g\xF6\
  r det f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:38.320740-06:00'
model: gpt-4-1106-preview
summary: "Att s\xF6ka och ers\xE4tta text \xE4r grundprocessen att hitta specifika\
  \ str\xE4ngar i data och byta ut dem mot andra str\xE4ngar. Programmerare g\xF6\
  r det f\xF6r att\u2026"
title: "S\xF6kning och ers\xE4ttning av text"
---

{{< edit_this_page >}}

## Vad & Varför?

Att söka och ersätta text är grundprocessen att hitta specifika strängar i data och byta ut dem mot andra strängar. Programmerare gör det för att effektivisera kodändringar, korrigera fel, eller uppdatera information i filer och program.

## Hur man gör:

I Fish Shell är det smidigt att söka och ersätta text med hjälp av inbyggda funktioner som `string`. Här är ett par exempel:

```fish
# Sök och ersätt 'gammalt' med 'nytt' i en sträng
echo 'Jag gillar det gammalt sättet' | string replace 'gammalt' 'nytt'

# Utdata: Jag gillar det nytt sättet

# Sök och ersätt tillämpat på varje rad i en fil
string replace 'färg' 'kulör' < gammal.txt > ny.txt
```

Observera att om du vill ersätta alla förekomster av en text, kan du lägga till flaggan `-a`.

```fish
# Sök och ersätt alla förekomster av 'äpple' med 'apelsin'
echo 'äpple + äpple = fler äpplen' | string replace -a 'äpple' 'apelsin'

# Utdata: apelsin + apelsin = fler apelsiner
```

## Djupdykning

Att söka och ersätta text i Fish Shell använder funktioner som `string replace`, vilket är en del av `string`-kommandon introducerade i version 2.3.0. Före det användes externa verktyg som `sed`. Alternativ till `string` kan inkludera dessa externa verktyg eller andra shell-program, men `string` är snabbare och enklare i Fish.

`string replace` är byggd för att vara intuitiv och kraftfull. Den stöder reguljära uttryck, hanterar binära data säkert och har utförliga flaggor som `-i` (case-insensitive) och `-r` (regex).

## Se även

- Fish Shell dokumentation om `string`: https://fishshell.com/docs/current/cmds/string.html
- Regex tutorial: https://www.regular-expressions.info/
- GNU `sed` manual: https://www.gnu.org/software/sed/manual/sed.html
