---
title:                "Stor bokstavering av sträng"
html_title:           "Gleam: Stor bokstavering av sträng"
simple_title:         "Stor bokstavering av sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att kapitalisera en sträng innebär att göra den första bokstaven i strängen till en stor bokstav, medan de resterande bokstäverna förblir små. Programmerare gör detta för att göra texten mer läsbar och tydligare för användare, speciellt när man t.ex. skriver rubriker eller namn.
## Skriv in kod:

```
Gleam.format.capitalize("programmering är kul") 
```
Denna kod ger utdata "Programmering är kul", där "P" är den stora bokstaven.
## Deep Dive
Kapitalisering av strängar har funnits i olika programmeringsspråk i flera år och är en vanlig funktion som används av många utvecklare. Det finns också alternativ för att göra "title case" eller "uppercase" på hela strängen. Implementationen av detta beror på programmeringsspråket och biblioteket som används, men vanligtvis innebär det att gå igenom strängen och ersätta den första bokstaven med en stor bokstav.
## Se även
- Dokumentation för Gleams `format` modul: https://gleam.run/packages/gleam_stdlib/latest/Gleam.Format
- Den här artikeln på engelska för mer information: [Gleam Programming: How To Capitalize a String](https://link.com/)