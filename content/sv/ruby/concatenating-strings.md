---
title:    "Ruby: Sammanslagning av strängar"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar kan vara en användbar teknik för att skapa dynamiska utskrifter eller beskrivande texter i ditt Ruby-program. Genom att lära dig denna funktion kan du öka flexibiliteten i dina kod och skapa mer interaktiva användargränssnitt.

## Så här gör du

För att sammanslå två strängar i Ruby, används plus-operanden (+) mellan dem. Till exempel:

```ruby
sträng1 = "Hej"
sträng2 = "Världen"

sammanslagen_sträng = sträng1 + sträng2
puts sammanslagen_sträng
```

Detta kommer att skriva ut "Hej Världen". Notera att när du lägger till en sträng till en annan, blir den slutgiltiga strängen längden av båda strängarna tillsammans.

```ruby
förnamn = "Sara"
efternamn = "Nilsson"

fullständigt_namn = förnamn + efternamn
puts fullständigt_namn
```

Detta kommer att skriva ut "SaraNilsson". För att lösa detta kan du lägga till ett mellanslag mellan strängarna.

```ruby
fullständigt_namn = förnamn + " " + efternamn
puts fullständigt_namn
```

Detta kommer nu att skriva ut "Sara Nilsson".

## Djupdykning

En annan användbar funktion vid sammanfogning av strängar är metoden `.concat()`. Den kan användas för att lägga till en sträng till en annan och är särskilt användbar när du arbetar med variabler som har tilldelats flera strängar.

```ruby
sträng1 = "Det här är en"
sträng2 = "konkatenerad sträng"

sträng1.concat(" ", sträng2)
puts sträng1
```

Detta kommer att skriva ut "Det här är en konkatenerad sträng".

En annan metod som är användbar för att sammanslå strängar är `.join()`. Den kan användas för att kombinera ett antal strängar eller array-element till en enda sträng.

```ruby
namn = ["Anna", "Lisa", "Johan"]

resultat = namn.join(" ")
puts resultat
```

Detta kommer att skriva ut "Anna Lisa Johan".

## Se även

* [Officiell Ruby dokumentation om String Concatenation](https://ruby-doc.org/core-2.6/String.html#method-i-%2B)
* [Ruby Guides om String Concatenation](https://www.rubyguides.com/2019/02/ruby-string-concatenation/)
* [Lär dig mer om Ruby på Codeacademy](https://www.codecademy.com/learn/learn-ruby)