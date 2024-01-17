---
title:                "Extrahera substrängar"
html_title:           "Ruby: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att extrahera substrängar i Ruby handlar om att hämta en del av en textsträng baserat på dess position eller ett visst mönster. Programerare gör detta för att effektivt hantera och manipulera data medan de utvecklar program och applikationer.

## Hur Man Gör:

Här är några exempel på hur man kan extrahera substrängar i Ruby, med en kort beskrivning av hur resultaten blir.

**Från början**
```ruby
"Hello World!"[0,5]
=> "Hello"
```

**Från slutet**
```ruby
"Hello World!"[-1,5]
=> "World"
```

**Med en specifik position och längd**
```ruby
"Hello World!"[6,5]
=> "World"
```

**Med regular expressions (regex)**
```ruby
"Hello World!"[/[a-z]+/]
=> "ello"
```

## Djupare Dyk:

### Historisk Kontext:
Extrahering av substrängar har varit en viktig del av programmering sedan början av datorer när textbearbetning var en av de främsta användningsområdena. Det är fortfarande en viktig funktion i moderna programmeringsspråk som Ruby.

### Alternativ:
En annan metod för att extrahera substrängar i Ruby är genom användning av `slice()`-metoden, som fungerar på ett liknande sätt men med lite annorlunda syntax.

### Implementation Detaljer:
I Ruby är substrängar representerade av `String`-objekt och kan manipuleras med hjälp av inbyggda metoder som `[]` och `slice()`. När substrängar extraheras från en textsträng orsakar det inte några förändringar i den ursprungliga strängen, utan returnerar en helt ny sträng.

## Se Även:

För mer information och exempel på hur man kan extrahera substrängar i Ruby, besök [Ruby's officiella dokumentation om strängar](https://ruby-doc.org/core-2.7.1/String.html#method-i-5B-5D).