---
title:    "Ruby: Länka samman strängar"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga, eller "concatenating", strängar är en praktisk funktion inom programmering som gör det möjligt att kombinera flera strängar till en enda. Detta kan vara användbart när vi behöver skapa dynamiska textsträngar eller när vi vill lägga till variabler i en sträng. Genom att lära sig hur man sammanfogar strängar kan man göra sina program mer flexibla och kraftfulla.

## Hur man gör

För att concatenating ska fungera måste vi använda ett operator som heter "plus", eller "+" på engelska. Om vi till exempel har två strängar, "Hej" och "världen", så kan vi använda "+" för att sammanfoga dem:

```Ruby
"Hej" + "världen" # output: "Hej världen"
```

Här kombinerar vi helt enkelt de två strängarna till en enda längre sträng. Det är viktigt att notera att strängarna måste vara omgivna av citattecken för att concatenating ska fungera. Om en av strängarna är en variabel så måste vi också använda återkallning inom citattecknen för att värdet ska ersättas i den slutliga strängen:

```Ruby
name = "Sara"

"Hej " + name # output: "Hej Sara"
```

Vi kan också använda "+=" för att lägga till en sträng i en befintlig variabel:

```Ruby
greeting = "Hej"
name = "Sara"

greeting += name # output: "Hej Sara"
```

## Deep Dive

En viktig sak att notera är att när vi sammanfogar två strängar så skapas en helt ny sträng, och de ursprungliga strängarna påverkas inte. Detta är särskilt viktigt att komma ihåg när man arbetar med variabler och vill ändra ett värde utan att påverka det ursprungliga värdet. Till exempel:

```Ruby
greeting = "Hej"
name = "Sara"

new_greeting = greeting + name # output: "Hej Sara"

puts new_greeting # output: "Hej Sara"
puts greeting # output: "Hej"
```

Vi kan också använda specialtecknet "<<" för att sammanfoga strängar, vilket gör att den ursprungliga strängen påverkas:

```Ruby
greeting = "Hej"
name = "Sara"

greeting << name # output: "HejSara"

puts greeting # output: "HejSara"
```

Det är också bra att känna till att när vi sammanfogar strängar så kan vi också använda variabler som innehåller andra datatyper, såsom siffror. I så fall kommer siffrorna att konverteras till strängar och sammanfogas som vanligt.

## Se också

- [Svensk Ruby-förening](https://www.ruby-lang.se/)
- [Officiell Ruby-dokumentation för strängar](https://ruby-doc.org/core-2.7.1/String.html)
- [Concatenating i andra programmeringsspråk](https://en.wikipedia.org/wiki/Concatenation_(programming))