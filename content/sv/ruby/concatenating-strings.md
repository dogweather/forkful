---
title:                "Sammanslagning av strängar"
html_title:           "Ruby: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Man kan vilja använda strings i Ruby för att skapa en mer effektiv och dynamisk kod. Genom att sammanfoga flera strings kan man skapa mer läsvärda uttryck och anpassa sin kod till olika situationer.

## Hur man gör
För att sammanfoga strings i Ruby kan man använda sig av operatorn `+`. Detta gör att man kan kombinera flera strings till en enda. Se nedan för ett exempel:

```Ruby
variable_1 = "Hej"
variable_2 = "världen!"
puts variable_1 + " " + variable_2
```

Detta kommer att ge utskriften "Hej världen!". Observera att mellanslaget mellan variablerna också måste läggas till för att undvika att orden hamnar ihop.

Man kan också använda sig av `<<` för att sammanfoga en string med en annan. Det finns även metoden `concat` som kan användas, men det är vanligare att använda `+` eller `<<`.

## Djupdykning
När man sammanfogar strings i Ruby skapas en ny string-objekt med en helt ny adress i minnet. Detta gör att man även kan ändra på den ursprungliga variabeln utan att det påverkar den nya. Se nedan för ett exempel:

```Ruby
vara = "Hej"
varb = vara
vara << " världen!"
puts vara # Utskrift: Hej världen!
puts varb # Utskrift: Hej
```

Det är även möjligt att sammanfoga flera variabler på en gång, som i exemplet nedan:

```Ruby
vara = "Hej"
varb = "världen"
varc = "!"
puts vara + " " + varb + varc # Utskrift: "Hej världen!"
```

Man kan också använda sig av interpolering för att sammanfoga strings. Då kan man använda variabler direkt i en string utan att behöva använda `+` eller `<<`. Se nedan för ett exempel:

```Ruby
vara = "Hej"
varb = "världen!"
puts "Jag säger #{vara} #{varb}" # Utskrift: Jag säger Hej världen!
```

## Se även
- [Officiell Ruby dokumentation](https://ruby-doc.org/core-3.0.2/String.html)
- [Ruby on Rails tutorial](https://www.railstutorial.org/book/beginning)