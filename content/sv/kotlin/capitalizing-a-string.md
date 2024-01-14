---
title:                "Kotlin: Att kapitalisera en sträng"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng kan vara användbart när man till exempel vill visa en titel eller ett namn på ett mer formellt sätt.

## Så här gör du

För att kapitalisera en sträng i Kotlin, kan du använda inbyggda String-metoder som `toUpperCase()` och `toLowerCase()`. Dessa metoder konverterar alla bokstäver till stora eller små, respektive.

Exempel 1:

```Kotlin
val namn = "anna andersson"
val kapitaliseratNamn = namn.capitalize()
println(kapitaliseratNamn) // Anna andersson
```

Exempel 2:

```Kotlin
val yrke = "doktor"
val merFormelltYrke = yrke.toUpperCase()
println(merFormelltYrke) // DOKTOR
```

Om du vill kapitalisera en sträng där varje ord börjar med en stor bokstav, kan du använda metoden `split()` först och sedan använda `capitalize()` för varje ord.

Exempel 3:

```Kotlin
val citat = "live and learn"
val kapitaliseratCitat = citat.split(" ")
    .map { it.capitalize() }
    .joinToString(" ")
println(kapitaliseratCitat) // Live And Learn
```

## Djupdykning

När du använder `capitalize()`-metoden, kommer endast första bokstaven i strängen att kapitaliseras. Alla andra bokstäver kommer att förbli som de är, vilket kan leda till oväntade resultat om du försöker kapitalisera ord som redan är helt eller delvis stora bokstäver.

En annan metod som du kan använda är `replaceFirstChar()` som tar emot en lambda-funktion som argument. Denna funktion kan du använda för att ändra första bokstaven i strängen till en stor bokstav och behålla alla andra bokstäver som de är.

Exempel 4:

```Kotlin
val land = "usa"
val kapitaliseratLand = land.replaceFirstChar { it.uppercase() }
println(kapitaliseratLand) // USA
```

Det finns också bibliotek som erbjuder mer avancerade funktioner för att kapitalisera strängar, särskilt när det kommer till att hantera olika språk och specialtecken.

## Se även

- [Officiell Dokumentation om String-metoder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Kapitalisera strängar i Kotlin: En handledning](https://blog.kotlin-academy.com/string-processing-part-i-using-regular-expressions-and-replace-all-to-transform-the-string-75252216d292)
- [Hantera specialtecken vid kapitalisering i Kotlin](https://medium.com/@fraggjkee/apply-kotlin-capitalize-with-locales-52179a122339)