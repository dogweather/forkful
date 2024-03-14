---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:24.459065-07:00
description: "Att skriva med stor bokstav inneb\xE4r att man \xE4ndrar den s\xE5 att\
  \ den f\xF6rsta bokstaven \xE4r versal och resten av str\xE4ngen \xE4r gemener.\
  \ Detta \xE4r en vanlig uppgift\u2026"
lastmod: '2024-03-13T22:44:38.318818-06:00'
model: gpt-4-0125-preview
summary: "Att skriva med stor bokstav inneb\xE4r att man \xE4ndrar den s\xE5 att den\
  \ f\xF6rsta bokstaven \xE4r versal och resten av str\xE4ngen \xE4r gemener. Detta\
  \ \xE4r en vanlig uppgift\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
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
