---
title:    "Fish Shell: Extrahera delsträngar"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Ibland vill vi bara extrahera en del av en längre textsträng för att få ut specifika delar av information. Det är här fiskskalsträngar kommer in i bilden. Med hjälp av substrings-funktionen kan vi enkelt plocka ut delar av en textsträng och använda dem för olika ändamål.

## Hur man gör

För att extrahera substrings med Fish Shell, kan vi använda oss av en inbyggd funktion som heter `string sub`. Syntaxen för denna funktion är:

```
Fish Shell

string sub -l <längd> -s <startposition> <text>
```

Låt oss säga att vi har en textsträng som heter `förnamn_efternamn` som innehåller ett förnamn och ett efternamn. Om vi bara vill extrahera förnamnet kan vi använda kommandot:

```
Fish Shell

string sub -l 8 -s 0 förnamn_efternamn
```

I detta fall är längden på förnamnet 8 tecken och vi börjar extrahera från position 0. Detta kommer att ge oss resultatet `förnamn_`.

Vi kan också använda oss av regexp för att extrahera delar av en textsträng. Om vi till exempel har en textsträng som heter `mobilnummer` och vill extrahera endast siffrorna i slutet, kan vi använda kommandot:

```
Fish Shell

string sub -r '[0-9]+$' mobilnummer
```

Detta kommer att ge oss resultatet `nummer`.

## Djupdykning

Funktionen `string sub` kan ta emot flera parametrar för att skräddarsy extraheringen av substrängen. Här är några vanliga användningsfall för dessa parametrar:

- `-l <längd>`: det här argumentet bestämmer hur många tecken som ska extraheras från textsträngen. Om inget värde anges, kommer hela textsträngen att extraheras.
- `-s <startposition>`: detta argument bestämmer från vilken position i textsträngen extraheringen ska börja. Om inget värde anges, börjar extraheringen från början av textsträngen.
- `-i`: detta argument gör extraheringen fallinsensitiv, vilket innebär att den inte tar hänsyn till skillnader i stora och små bokstäver.
- `-r <regex>`: med regexp kan vi använda oss av mönster för att extrahera mer specifika delar av en textsträng.

## Se även

- [Fiskskalsträngar dokumentation](https://fishshell.com/docs/current/cmds/string.html#string-sub)
- [Fiskskal regex dokumentation](https://fishshell.com/docs/current/cmds/string.html#string-match)
- [Fish Shell hjälpforums](https://github.com/fish-shell/fish-shell/issues)