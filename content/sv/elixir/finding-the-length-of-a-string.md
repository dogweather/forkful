---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng innebär att räkna antalet tecken det innehåller. Som programmerare gör vi detta för att hantera data mer effektivt och göra korrekthetskontroll av input.

## Hur man gör:

Använd `String.length/1`-funktionen så här:

```elixir
IO.puts String.length("Hej, världen") 
```

Koden ovan kommer att skriva ut `12`, som är antalet tecken i strängen `"Hej, världen"`.

## Djupdykning

Historiskt sett använder många programmeringsspråk en approach där varje tecken representeras av ett fast antal bytes. Men Elixir, en Unicode-kompatibel språk, tar en annan approach.

Alternativt kan grafemfunktionen `String.graphemes/1` användas för att räkna antalet grafemer istället för kodpunkter. Notera dock att det tar längre tid än `String.length/1`:

```elixir
IO.puts String.length("hejä") 
```

Resultatet kommer att bli `4`, medan det blir `5` om du använder `byte_size/1` eftersom "ä" tar upp två bytes i UTF-8. 

Funktionen `String.length/1` i Elixir är baserat på UTF-8 kodade binärer och fungerar genom att iterera över den binära representationen av strängen och räkna kodpunkter, inte den faktiska bytestorleken.

## Se också

1. Officiellt dokument för [String-modul](https://hexdocs.pm/elixir/String.html).

2. Djupare förståelse för [UTF-8 kodning](https://unicode.org/faq/utf_bom.html).

3. Elixir-forumtråd om när man använder [byte_size mot string_length](https://elixirforum.com/t/byte-size-vs-string-length/12032).