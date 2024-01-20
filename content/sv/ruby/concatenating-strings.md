---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad och varför?

Sammanslagning av strängar i programmering innebär att man kombinerar två eller flera strängar till en enda sträng. Programmerare gör detta för att manipulera eller presentera data på ett mer läsbart och effektivt sätt.

## Hur gör man:

I Ruby kan vi använda plusmetoden "+" eller "<<" för att slå samman strängar. Här är ett exempel:

```Ruby
str1 = "Hej "
str2 = "världen!"
puts str1 + str2
```

Utgången blir: "Hej världen!"

Om vi vill lägga till en sträng till en existerande sträng utan att skapa en ny sträng, så kan vi använda "<<" metoden:

```Ruby
str1 = "Hej"
str1 << " världen!"
puts str1
```

Utgången blir återigen: "Hej världen!"

## Djupdykning:

Historiskt sett användes plusmetoden (+) mest för att slå samman strängar, men det leder till skapandet av nya strängar, vilket kan vara kostsamt när man jobbar med stora mängder data eller inom prestandakritiska applikationer. Ruby introducerade "<<" som är mer minnesvänlig än "+" eftersom den lägger till den andra strängen till den ursprungliga strängen snarare än att skapa en helt ny sträng.

En annan teknik som kan användas är `#concat` metoden, som fungerar precis som "<<" metoden.

```Ruby
str1 = "Hej"
str1.concat(" världen!")
puts str1
```

Utgången blir fortfarande: "Hej världen!"

Beroende på din kods specifika krav kan du välja vilken metod som passar bäst för att sammanslå strängar.

## Se också:

Vidare läsning om strängsammansättning och alternativa metoder kan hittas i följande källor:

1. Ruby Documentation - concat: https://ruby-doc.org/core-2.7.0/String.html#method-i-concat
2. Ruby Documentation - << : https://ruby-doc.org/core-2.5.0/String.html#method-i-3C-3C
3. StackOverflow Discussion on String Concatenation: https://stackoverflow.com/questions/4684446/why-is-the-shovel-operator-preferred-over-plus-for-concatenating-ruby
4. Concatenating Strings in Ruby: https://www.rubyguides.com/2018/06/ruby-string-concatenation/