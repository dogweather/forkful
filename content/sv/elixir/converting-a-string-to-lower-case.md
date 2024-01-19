---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till små bokstäver innebär att ändra alla stora bokstäver i en textsträng till motsvarande små bokstäver. Programmerare gör detta för att undvika oönskad skillnad mellan fall vilket underlättar jämförelse och sökning av strängar.

## Så här gör man:

I Elixir gör du detta med `String.downcase` funktionen, som visat nedan:

```elixir
IO.puts(String.downcase("HeJ VärLdeN")) 
```
Detta kommer att skriva ut följande på terminalen:
```
hej världen
```

## Djupdykning

- Historiskt sett, kom denna koncept från behovet av att normalisera data för att undvika inkonsekventa sökresultat. Till exempel, i databaser, skulle "Elixir" och "elixir" vara olika strängar om inte jämförelsen var fallkänslig.

- Ett alternativ till `String.downcase` är att använda Erlangs inbyggda BIF (builtin function) `:string.to_lower/1`, men det är mer komplicerat och erbjuder ingen extra fördel.

- `String.downcase` använder Unicode Case Mapping för att korrekt ändra fall på strängar, inklusive icke-latinska tecken.

## Se också

- [Elixir's String Module Documentation](https://hexdocs.pm/elixir/String.html#downcase/2)
- [On Unicode Case Mapping](https://www.unicode.org/versions/Unicode13.0.0/ch03.pdf)
- [Erlang's string Module Documentation](http://erlang.org/doc/man/string.html)