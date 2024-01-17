---
title:                "Extrahera substrängar"
html_title:           "Elixir: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar är en typ av textmanipulation där man tar ut en del av en befintlig sträng och sparar den som en egen sträng. Detta är vanligt inom programmering för att kunna hantera och bearbeta stora mängder av data eller för att göra specifika sökningar i en text.

## Hur man gör:
```Elixir
sträng = "Hej från Sverige"
delsträng = String.slice(sträng, 4..7)
# delsträng blir nu "från"
```

## Djupdykning:
Det finns flera olika sätt att extrahera delsträngar i Elixir, beroende på vilka specifika behov man har. Ett alternativ till ovanstående exempel är att använda funktionen `String.split_at`, som tar in en sträng och en position och delar strängen i två delar där positionen ligger. Man kan även använda regular expressions för att söka efter specifika mönster i en text och sedan extrahera delsträngar baserat på dessa mönster.

## Se även:
- Officiell dokumentation för delsträngar i Elixir: https://hexdocs.pm/elixir/String.html#slice/3
- En mer djupgående förklaring av hur delsträngar fungerar: https://blog.nodegear.com/extracting-substrings-in-elixir/