---
title:                "Ta bort tecken som matchar ett mönster"
date:                  2024-01-20T17:42:12.328392-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar ett mönster innebär att filtrera ut specifika tecken från en sträng baserat på definierade kriterier. Programmerare gör detta för att rensa data, validera input eller förbereda strängar för efterföljande bearbetning.

## Hur man gör:
```Gleam
import gleam/regex

pub fn remove_matching_characters(pattern: String, from: String) -> String {
  let re = regex.compile(pattern).expect("Invalid regex pattern")
  regex.replace(re, from, "")
}

fn main() {
  let cleaned_string = remove_matching_characters("[aeiou]", "Hello, Gleam!")
  println(cleaned_string) // Skriver ut: "Hll, Glm!"
}
```

## Djupdykning:
Historiskt sett har behovet av att manipulera strängar och radera visst innehåll varit en del av programmeringen sedan början. I Gleam och de flesta moderna språk använder vi reguljära uttryck (regex) för detta. Alternativ till regex inkluderar att manuellt iterera över strängar och välja vad som ska behållas eller tas bort, vilket kan vara mer prestanda effektivt för enkla mönster men mindre flexibelt. Implementering av regex i Gleam är lik den i Erlang, vilket innebär pålitlighet och effektivitet tack vare Erlangs långvariga utveckling.

## Se också:
- Regular expressions in depth: [Regular-Expressions.info](https://www.regular-expressions.info/)
- Erlang's regex documentation for a deeper understanding of the underlying implementation: [Erlang Regex Module](http://erlang.org/doc/man/re.html)