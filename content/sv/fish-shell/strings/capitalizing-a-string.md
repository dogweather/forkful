---
title:                "Gör om en sträng till versaler"
date:                  2024-02-03T19:05:24.459065-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva med stor bokstav innebär att man ändrar den så att den första bokstaven är versal och resten av strängen är gemener. Detta är en vanlig uppgift inom textbehandling, normalisering av användarinmatning och dataformatering för att säkerställa konsekvens eller för att möta specifika formateringskriterier.

## Hur man gör:

I Fish Shell kan strängar manipuleras direkt med inbyggda funktioner, utan behov av externa verktyg eller bibliotek. För att göra en sträng med stor börjanbokstav kan du kombinera `string`-kommandot med underkommandon.

```fish
# Exempelsträng
set sample_string "hello world"

# Gör första bokstaven stor
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Utdata:
```
Hello world
```

För scenarion som kräver att flera ord i en sträng görs med stor bokstav (t.ex. konvertera "hello world" till "Hello World"), skulle du iterera över varje ord och tillämpa logiken för stor bokstav på varje:

```fish
# Exempelmening
set sentence "hello fish shell programming"

# Gör varje ord med stor bokstav
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Sammanfoga de ord med stor bokstav
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Utdata:
```
Hello Fish Shell Programming
```

Notera att Fish Shell inte direkt erbjuder en strategi med ett enda kommando för att göra en hel mening med stor bokstav på samma sätt som vissa programmeringsspråk gör med sina strängmetoder. Därför representerar kombinationen av `string split`, `string sub`, `string upper`, och sedan återförening ett idiomatiskt tillvägagångssätt i Fish Shell för att uppnå detta.
