---
title:                "Kotlin: Hitta längden av en sträng"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden av en sträng är en viktig funktion som är avgörande för många program och applikationer. Det hjälper till att effektivisera och förbättra hanteringen av textdata, vilket är avgörande för många programmerares arbete.

## Hur man gör det

```Kotlin
// Skapa en variabel som innehåller en sträng
var str: String = "Hej, hur mår du?"

// Använd funktionen length för att få längden av strängen
var length: Int = str.length

// Skriv ut resultatet
print(length)

```

Output: 15

Som vi kan se i exemplet ovan används funktionen "length" för att få längden av en sträng. Detta är en funktion som finns tillgänglig i Kotlin Standardbibliotek, så du behöver inte skriva kod från grunden.

## Djupdykning

Det finns flera sätt att hitta längden av en sträng i Kotlin. En annan vanlig metod som också finns tillgänglig i Standardbiblioteket är "count()". Den används på samma sätt som "length", men räknar antalet tecken istället för att ge den faktiska längden.

En annan viktig aspekt att notera är att i Kotlin anses strängar vara immutabla, vilket betyder att de inte kan ändras. Därför är funktionen "length" och "count" båda snabba, eftersom de endast behöver räkna och inte behöver ändra den ursprungliga strängen.

## Se även

- [Kotlin Standardbibliotek](https://kotlinlang.org/api/latest/jvm/stdlib/)

- [Kotlin Text Utilities](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/#length)

- [Kotlin String Extensions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/#length)