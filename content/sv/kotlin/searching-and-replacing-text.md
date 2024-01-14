---
title:    "Kotlin: Söka och ersätta text"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att byta ut text i en kod kan vara en tidsbesparande åtgärd som kan hjälpa dig att effektivisera ditt arbete. Det kan vara särskilt användbart om du behöver uppdatera flera delar av din kod, istället för att göra det manuellt en efter en.

## Så här gör du

För att byta ut text i Kotlin, kan du använda funktionen `replace()` som finns inbyggd i språket. Ta en titt på följande exempel:

```Kotlin
val text = "Det här är en text som behöver bytas ut"
val nyText = text.replace("behöver bytas ut", "är utbytt")

print(nyText)
```

Output: Det här är en text som är utbytt

I detta exempel har vi definierat en variabel `text` som innehåller den ursprungliga texten. Sedan använder vi `replace()` funktionen och anger vilken del av texten som behöver bytas ut och vad det ska ersättas med. Slutligen skriver vi ut den nya texten som lagrats i variabeln `nyText`.

## Djupdykning

För mer komplexa situationer, kan du använda reguljära uttryck för att söka och ersätta text i Kotlin. Dessa mönster gör det möjligt att söka efter mer specifikt innehåll och göra mer avancerade utbyten. Låt oss ta en titt på följande exempel:

```Kotlin
val text = "Det här är en text med både sifferkombinationer 123 och bokstäver abc"

val nyText = text.replace(Regex("[0-9]"), "x")

print(nyText)
```

Output: Det här är en text med både sifferkombinationer xxx och bokstäver abc

Här använder vi `Regex()` funktionen för att skapa ett reguljärt uttryck som söker efter alla siffror i texten och byter ut dem med bokstaven "x". Detta är bara ett exempel på vad reguljära uttryck kan göra, men de kan vara mycket kraftfulla verktyg när det gäller att söka och ersätta text i din kod.

## Se också

- [Kotlin-docs: String.replace()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)
- [Kotlin-docs: Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)