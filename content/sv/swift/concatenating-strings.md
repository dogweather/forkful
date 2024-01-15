---
title:                "Sammanslagning av strängar"
html_title:           "Swift: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Det finns många tillfällen där du behöver kombinera flera olika textsträngar för att skapa en större och mer komplett sträng. Det kan till exempel vara för att visa användarinmatningar, skapa rapporter eller skapa anpassade meddelanden. Att kunna koppla ihop strängar är ett viktigt koncept att förstå inom Swift-programmering och kan hjälpa dig att göra din kod mer effektiv och läsbar.

## Hur man gör
För att sammanslå två eller flera strängar i Swift använder du operatorn "+" mellan strängarna. Här är ett exempel på hur du kan lägga ihop två strängar och skriva ut resultatet:

```Swift
let förnamn = "John"
let efternamn = "Doe"
let fullständigtNamn = förnamn + " " + efternamn
print(fullständigtNamn)
```

Resultatet av koden ovan kommer att vara "John Doe". Observera att vi använder ett mellanslagsträng mellan förnamn och efternamn för att skapa ett mellanrum i den sammanslagna strängen.

Du kan också använda operatorn "+=" för att lägga till en sträng till en befintlig sträng. Till exempel:

```Swift
var hälsning = "Hej"
hälsning += " världen"
print(hälsning)
```

Resultatet blir "Hej världen", där "världen" har lagts till efter "Hej".

## Djupdykning
När du sammanslår strängar i Swift är det viktigt att vara medveten om hur strängarna hanteras. Swift använder sig av Unicode för att hantera strängar, vilket gör det möjligt att inkludera många olika tecken och språk i en sträng. Det innebär också att en sammanslagen sträng kan bestå av tecken från olika språk och alfabet.

Det finns också andra sätt att sammanslå strängar i Swift, som att använda metoder som "append" och "append(contentsOf:)" för att lägga till strängar till en befintlig sträng. Dessutom finns det även en metod som heter "join" som kan användas för att sammanslå flera strängar med ett separator-tecken mellan dem.

En annan viktig sak att komma ihåg är att sammanslagna strängar är oföränderliga, vilket innebär att du inte kan ändra en befintlig sträng genom att lägga till en annan sträng till den. Istället skapar du en helt ny sträng när du sammanslår dem.

## Se även
* [String Operations in Swift](https://medium.com/@aleksandar_raf/why-swift-extensions-are-super-useful-7da8dd7ba57d)
* [Working with Strings in Swift](https://learnappmaking.com/swift-string-how-to/)
* [Concatenation in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-join-strings-to-make-a-single-string)