---
title:                "Elixir: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Elixir är ett populärt programspråk som baseras på funktionsprogrammering. Det har enkla syntax, bra prestanda, och stöd för parallellt och distribuerat arbete. En av dess många inbyggda funktioner är möjligheten att konvertera en sträng till små bokstäver, vilket kan vara användbart i många olika situationer.

## Hur man gör

För att konvertera en sträng till små bokstäver i Elixir kan du använda funktionen `String.downcase`. Detta kommer att ändra alla bokstäver i strängen till deras motsvarande små bokstäver. Se nedan för ett exempel:

```elixir
sträng = "HEJ VÄRLDEN"
puts String.downcase(sträng)
```

Detta kommer att producera följande output:
```elixir
hej världen
```

## Djupdykning

När du använder funktionen `String.downcase`, finns det några saker att tänka på. För det första är det viktigt att notera att denna funktion endast fungerar med ASCII-karakterer. Om du har strängar med icke-ASCII-karakterer, måste du använda funktionen `String.downcase_utf8` istället.

För det andra är det viktigt att notera att funktionen `String.downcase` returnerar en ny sträng istället för bara att modifiera originalsträngen. Detta innebär att du behöver tilldela resultatet av funktionen till en variabel om du vill använda den nya strängen.

Slutligen, det kan vara användbart att veta att det finns en motsvarande funktion `String.upcase` som konverterar en sträng till stora bokstäver.

## Se även

- [Elixir String modul](https://hexdocs.pm/elixir/String.html)
- [Funktionsprogrammering med Elixir](https://medium.com/the-elixir-hub/functional-programming-with-elixir-c253093775d9)