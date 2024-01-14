---
title:                "Ruby: Sammanslagning av strängar"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att lära sig hur man sammanfogar strängar i Ruby är ett viktigt koncept för att kunna skapa dynamiska och interaktiva program. Genom att kombinera flera strängar kan du skapa mer användbara och föränderliga meddelanden och utskrifter i ditt program.

## Så här gör du

För att sammanfoga strängar i Ruby kan du använda operatorn "+" eller metoden "concat". Här är ett enkelt exempel:

```Ruby
first_name = "Lisa"
last_name = "Svensson"

puts first_name + " " + last_name
```

Output: Lisa Svensson

För att undvika att behöva skriva "+" flera gånger, kan du använda metoden "concat" istället:

```Ruby
first_name = "Lisa"
last_name = "Svensson"

puts first_name.concat(" ", last_name)
```

Output: Lisa Svensson

Det finns även andra sätt att sammanfoga strängar, som att använda "<<" eller interpolering med "#{}". Läs mer om dessa metoder och deras syntax i dokumentationen för Ruby.

## Deep Dive

När vi sammanfogar strängar i Ruby skapar vi egentligen en ny sträng istället för att ändra på de befintliga strängarna. Detta beror på att strängar är oföränderliga, vilket betyder att de inte kan ändras efter att de har skapats. Därför är det viktigt att förstå skillnaden mellan att ändra en sträng och att skapa en ny.

En annan viktig sak att komma ihåg när det gäller sammanfogning av strängar är att många operationer i Ruby returnerar en ny sträng istället för att ändra på den befintliga. Detta gäller särskilt om du använder metoden "split" för att dela en sträng i flera delar.

## Se också

* Dokumentation för Ruby - https://www.ruby-lang.org/sv/documentation/
* En guide till sammanfogning av strängar i Ruby - https://www.rubyguides.com/2017/01/ruby-string-concatenation/
* Mer om oföränderliga objekt i Ruby - https://medium.com/rubyinside/the-surprising-and-quite-deliberate-things-you-should-know-about-ruby-swap-cd4682f7dede