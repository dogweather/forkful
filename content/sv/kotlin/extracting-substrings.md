---
title:                "Kotlin: Extrahering av delsträngar"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I många olika programmeringsspråk finns det en funktion för att ta ut delsträngar, dvs. en del av en sträng. Denna funktion kan vara användbar i många olika situationer, t.ex. för att manipulera data eller extrahera specifika delar av en textsträng. I Kotlin finns det också möjlighet att göra detta genom några enkla steg.

## Så här gör du

För att extrahera en delsträng i Kotlin använder man funktionen `substring()`. För att göra detta behöver man först ha en sträng som man vill extrahera en del av. Sedan anger man vilket index man vill börja på och vilket index man vill sluta på. Till exempel om vi har strängen "Hej världen" och vill extrahera ordet "världen" kan vi göra det på följande sätt:

```Kotlin
val text = "Hej världen"
val delsträng = text.substring(4,11)
print(delsträng) //output: världen
```

Man kan även ange enbart ett startindex och då kommer funktionen extrahera slutet av strängen från och med detta index. Till exempel om vi istället bara vill extrahera "världen" från samma sträng som ovan kan vi göra det såhär:

```Kotlin
val delsträng = text.substring(4)
print(delsträng) //output: världen
```

Man kan också använda `substring()` för att extrahera en delsträng baserat på ett visst villkor. Till exempel om vi har en lista med namn på personer och vi bara vill ha ut de som börjar på bokstaven "A", kan vi göra såhär:

```Kotlin
val namnlista = listOf("Adam", "Bertil", "Anna", "Carin")
val aNamn = namnlista.filter { it.startsWith("A") }
print(aNamn) //output: [Adam, Anna]
```

## Djupdykning

Förutom att kunna användas för att extrahera delar av en sträng, kan man också göra mer avancerade saker med `substring()` i Kotlin. Till exempel kan man använda det för att ändra en del av en sträng. Om vi har en sträng med en telefonnummer och vill byta ut de fyra sista siffrorna till "****", kan vi göra det såhär:

```Kotlin
var telefon = "070-123 45 67"
telefon = telefon.substring(0,7) + "****"
print(telefon) //output: 070-123 ****
```

Man kan också använda `substring()` för att ta bort en del av en sträng genom att helt enkelt inte inkludera den del man vill ta bort i slutindexet. Till exempel om vi vill ta bort prefixet "070-" från vårt telefonnummer kan vi göra såhär:

```Kotlin
telefon = telefon.substring(4)
print(telefon) //output: 123 ****
```

## Se även

- Kotlin Official Documentation: https://kotlinlang.org/docs/reference/strings.html#substring
- StackOverflow: https://stackoverflow.com/questions/50200856/better-way-to-extract-a-part-of-string-in-kotlin