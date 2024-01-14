---
title:    "Elixir: Söka och ersätta text"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

När du arbetar med Elixir-programmering, kan det ibland vara nödvändigt att söka och ersätta text i din kod. Detta kan vara till nytta om du vill snabba upp processen att uppdatera flera filer eller för att göra en snabb ändring i koden.

## Så här gör du

För att söka och ersätta text i Elixir, kan du använda funktionen "String.replace". Här är ett exempel på hur du kan söka och ersätta en sträng i en lista:

```Elixir

lista = ["Hej", "världen", "hej"]

lista
|> Enum.map(fn(x) -> String.replace(x, "hej", "Hej då") end)

# Output: ["Hej", "världen", "Hej då"]

```

Som du kan se i exemplet ovan, använde vi funktionen "String.replace" för att söka efter "hej" och ersätta det med "Hej då". Denna funktion tar tre argument: den ursprungliga strängen, den sökta texten och den ersättningssträngen.

## Djupdykning

Förutom funktionen "String.replace" finns det också andra hjälpsamma funktioner för sökning och ersättning i Elixir. Till exempel kan du använda "String.replace_leading" respektive "String.replace_trailing" om du bara vill ersätta text i början eller slutet av en sträng.

Det finns också möjlighet att använda reguljära uttryck (regex) för att söka efter och ersätta text. Du kan göra detta genom att använda "Regex.replace". Det är bra att lära sig använda regex i Elixir, eftersom det är en kraftfull funktion för sökning och ersättning.

## Se även

Om du vill ha mer information om sökning och ersättning i Elixir, kan du kolla in dessa länkar:

- Officiell Elixir dokumentation för String-modulet: https://hexdocs.pm/elixir/String.html
- Bra tutorial om användning av reguljära uttryck (regex) i Elixir: https://adoptingerlang.org/docs/elixir-guide/understanding-regular-expressions/

Förhoppningsvis har denna guide gett dig en grundläggande förståelse för hur du kan söka och ersätta text i Elixir. Lycka till med din kodning!