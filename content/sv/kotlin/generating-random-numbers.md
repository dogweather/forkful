---
title:                "Kotlin: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig uppgift inom programmering och kan ha många användningsområden. Det kan användas för att skapa slumpmässiga listor, simulera spel eller testa olika algoritmer. Oavsett anledning, är kunskap om hur man genererar slumpmässiga nummer en bra färdighet att ha som programmerare.

## Hur man gör det

För att generera slumpmässiga nummer i Kotlin, kan vi använda funktionen `random()` som är tillgänglig i standardbiblioteket. Denna funktion returnerar ett slumpmässigt decimaltal mellan 0 och 1.

```Kotlin
val randomNum = Math.random()
println(randomNum)
```

För att få ett heltal, kan vi använda `nextInt()` funktionen som finns i `Random` klassen. Denna funktion tar in ett argument för att sätta ett övre gränsvärde för det slumpmässiga numret.

```Kotlin
val randomInt = Random().nextInt(10)
println(randomInt)
```

Vi kan även generera slumpmässiga nummer inom ett givet intervall genom att använda `nextInt()` tillsammans med `+ 1` för att inkludera det övre gränsvärdet.

```Kotlin
val randomNumInRange = Random().nextInt(10) + 1
println(randomNumInRange)
```

Slutligen, för att få ett slumpmässigt Boolean-värde, kan vi använda `nextBoolean()` funktionen i `Random` klassen.

```Kotlin
val randomBool = Random().nextBoolean()
println(randomBool)
```

## Djupdykning

Det finns flera olika algoritmer som kan användas för att generera slumpmässiga nummer, men en vanlig metod är den så kallade "linear congruential generator" (LCG). Det är en enkel och effektiv algoritm som använder en formel för att beräkna nästa slumpmässiga nummer baserat på det föregående numret. Detta gör den snabbare än andra algoritmer som använder en tabell för att generera nummer.

Det är dock viktigt att notera att LCG kan leda till en viss bias i distributionen av slumpmässiga nummer och bör därför inte användas för kryptografiska ändamål.

## Se även

För mer information om generering av slumpmässiga nummer i Kotlin, rekommenderar vi att läsa dokumentationen för `Random` klassen och `Math` objektet.

[Dokumentation för Random klassen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)

[Dokumentation för Math objektet](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.math/index.html)