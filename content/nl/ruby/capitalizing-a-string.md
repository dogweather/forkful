---
title:                "Een string met hoofdletters maken"
aliases:
- nl/ruby/capitalizing-a-string.md
date:                  2024-01-28T21:55:34.354798-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekenreeks kapitaliseren betekent het eerste karakter naar hoofdletter omzetten en de rest naar kleine letters. Programmeurs doen dit om de uitvoer te formatteren voor consistentie of om aan bepaalde gegevensnormen te voldoen.

## Hoe:

In Ruby kapitaliseer je een tekenreeks met de methode `.capitalize`:

```Ruby
puts "hello world".capitalize  # Uitvoer: "Hello world"
```

Om alle woorden in een tekenreeks te kapitaliseren, gebruik:

```Ruby
puts "hello world".split.map(&:capitalize).join(' ')  # Uitvoer: "Hello World"
```

Merk op dat `.capitalize` alleen het eerste woord beïnvloedt:

```Ruby
puts "hello WORLD".capitalize  # Uitvoer: "Hello world"
```

## Diepere Duik

Het kapitaliseren van tekenreeksen is noodzakelijk geworden sinds computers begonnen te communiceren met mensen. Het zorgt ervoor dat eigennamen en zinnen correct beginnen, volgens de grammaticanormen.

In sommige talen, zoals Ruby, is `.capitalize` ingebouwd. Anderen hebben aangepaste functies of bibliotheken nodig. De methode in Ruby maakt ook de rest van de tekenreeks klein, zoals te zien is in de bovenstaande voorbeelden.

Een alternatief in Ruby is het gebruik van de methode `titleize` uit de `ActiveSupport::Inflector` methoden, meestal gebruikt in Rails:

```Ruby
require 'active_support/core_ext/string/inflector'
puts "hello world".titleize  # Uitvoer: "Hello World"
```

Echter, `titleize` is zwaarder en geen onderdeel van Ruby's standaardbibliotheek.

Wat de implementatie betreft, wanneer je `.capitalize` aanroept, maakt Ruby een nieuwe tekenreeks met het eerste karakter omgezet naar hoofdletter en de rest naar kleine letters. Het is handig om te zorgen voor consistente opmaak in gebruikersinterfaces en gegevensverwerking.

## Zie Ook

- Ruby's documentatie over `.capitalize`: [Ruby Docs - capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- Over `ActiveSupport::Inflector` en `titleize`: [API Dock - titleize](https://apidock.com/rails/String/titleize)
- Om te leren over Ruby's andere tekenreeksmethoden: [Ruby Docs - String](https://ruby-doc.org/core-2.7.0/String.html)
