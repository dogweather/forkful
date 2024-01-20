---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att sammanfoga strängar, eller "konkatenering", innebär att sätta två eller flera strängar ihop som en. Programmerare gör detta för att bygga upp och manipulera text på dynamiska sätt.

## Hur man gör:
I Elixir finns flera sätt att sammanfoga strängar, men det vanligaste sättet är att använda `<>` operanden. Nedan är några exempel:

```Elixir
s1 = "Hej, "
s2 = "världen!"

s3 = s1 <> s2 
IO.puts(s3) # Skriver ut "Hej, världen!"
```
Du kan även använda `String.concat/2` funktionen:

```Elixir
s1 = "Hej, "
s2 = "världen!"

s3 = String.concat(s1, s2)
IO.puts(s3) # Skriver ut "Hej, världen!"
```

## Djupdykning
String konkatenering har historiskt använts i alla programmeringsspråk och är en grundläggande del av att interagera med text. I tidiga språk, som C, kunde detta vara klurigt och potentiellt farligt på grund av direkt minneshantering.

Elixir är vänligare, men det finns alternativ till `<>` och `String.concat/2`. Till exempel, kan vi ansluta listor av strängar med `Enum.join/2`:

```Elixir
delar = ["Hej, ", "världen!"]

hela = Enum.join(delar) 
IO.puts(hela) # Skriver ut "Hej, världen!"
```

Det är nödvändigt att notera att `<>` lättare förstås av läsaren att sammanfogning av två strängar sker, medan `String.concat/2` och `Enum.join/2` är mer uttrycksfulla för sammanfogning av flera strängar.

## Se även
1. Elixir officiella dokumentation på strängar: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
2. Elixir School för mer djupgående guide om strängar i Elixir: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)