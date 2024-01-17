---
title:                "Interpolera en sträng"
html_title:           "Ruby: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

String-interpolation är när man blandar variabler och text i en sträng för att få en dynamisk sträng som ändras baserat på variablernas värden. Det är ett vanligt verktyg för programmerare för att skapa dynamiska strängar som kan anpassas till olika situationer och behov.

## Så här gör du:

För att interpolera en sträng i Ruby använder man #{} runt variabelnamnet inuti en dubbelcitering. Detta säger till Ruby att värdet på variabeln ska användas i strängen istället för själva variabelnamnet. Här är ett exempel:

```ruby
name = "Lisa"
puts "Hej #{name}! Välkommen till Ruby världen."
```

Detta skulle skriva ut "Hej Lisa! Välkommen till Ruby världen." eftersom värdet av variabeln "name" ersätter #{name} i strängen.

## Djupdykning:

Interpolering av strängar blev populärt på grund av dess enkelhet och flexibilitet jämfört med andra metoder som string concatenation. Det finns dock andra sätt att interpolera strängar i Ruby, som att använda metoden sprintf(). Det finns också olika former av string-interpolation, som single-quoted och percent literals.

## Se även:

För mer information om string-interpolation i Ruby, se följande länkar:

- [Officiell Ruby dokumentation](https://ruby-doc.org/core-2.7.1/doc/syntax/literals_rdoc.html#label-Percent+Strings)
- [Ruby Guides](https://www.rubyguides.com/2016/08/ruby-string-interpolation/)