---
title:                "Att hitta längden på en sträng."
html_title:           "Ruby: Att hitta längden på en sträng."
simple_title:         "Att hitta längden på en sträng."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna längden på en sträng är en grundläggande färdighet inom programmering, oavsett vilket språk man använder. Det är en viktig kunskap att ha när man arbetar med textbaserade data eller behöver manipulera strängar på olika sätt.

## Hur Man Gör
Beräkna längden på en sträng i Ruby är väldigt enkelt. Du kan använda metoden "length" på en sträng-variabel för att få fram antalet tecken i strängen. Här är ett exempel:

```Ruby
my_string = "Hej världen!"
puts my_string.length
```

Det här koden kommer att skriva ut 13, eftersom det är antalet tecken i strängen "Hej världen!". Du kan också använda metoden "count" för att räkna ett specifikt tecken i en sträng. Till exempel:

```Ruby
my_string = "Peter Pan"
puts my_string.count("a") # skriver ut 1, eftersom det bara finns en "a" i strängen
```

## Djupdykning
I Ruby är en sträng egentligen bara en samling av tecken, vilket är anledningen till att vi kan använda metoden "length". När en sträng skapas, tilldelas varje tecken en position eller index. Därför kan vi också använda metoden "[]", tillsammans med ett tal, för att få fram ett specifikt tecken i en sträng. Till exempel:

```Ruby
my_string = "abcde"
puts my_string[1] # skriver ut "b" eftersom det är tecknet på position 1 i strängen
```

En annan intressant egenskap hos strängar i Ruby är att de är "mutable", vilket betyder att de kan ändras. Det här kan vara användbart när man behöver ändra en del av en sträng, till exempel genom att byta ut ett tecken eller ta bort en del av strängen. Här är några exempel på hur man kan ändra en sträng:

```Ruby
my_string = "Hej världen!"
my_string[0] = "S" # bytar ut "H" mot "S", strängen är nu "Sej världen!"
puts my_string[0..3] # skriver ut "Sej ", eftersom vi hämtar tecknen på position 0 till 3
```

## Se Också
- [Ruby String Dokumentation](https://ruby-doc.org/core-3.0.0/String.html)
- [Grundläggande Programmeringstermer](https://www.codingbrains.com/basic-programming-terminologies-glossary)