---
date: 2024-01-26 00:54:53.285051-07:00
description: "Hur man g\xF6r: Kotlin st\xF6djer `try`, `catch`, `finally` och `throw`\
  \ f\xF6r att hantera fel. S\xE5 h\xE4r anv\xE4nder du dem."
lastmod: '2024-03-13T22:44:37.878666-06:00'
model: gpt-4-1106-preview
summary: "Kotlin st\xF6djer `try`, `catch`, `finally` och `throw` f\xF6r att hantera\
  \ fel."
title: Hantering av fel
weight: 16
---

## Hur man gör:
Kotlin stödjer `try`, `catch`, `finally` och `throw` för att hantera fel. Så här använder du dem:

```Kotlin
fun main() {
    val täljare = 10
    val nämnare = 0

    try {
        val resultat = täljare / nämnare
        println("Resultat: $resultat")
    } catch (e: ArithmeticException) {
        println("Kan inte dela med noll, kompis.")
    } finally {
        println("Detta händer oavsett vad.")
    }
}
```

Utmatning:
```
Kan inte dela med noll, kompis.
Detta händer oavsett vad.
```

Om något går fel i `try`-blocket, hoppar exekveringen till `catch`. Den fångar det specifika felet som kastats (i det här fallet `ArithmeticException`). `finally`-blocket körs efteråt—oavsett utfall.

## Fördjupning
`try-catch`-blocket har funnits sedan programmeringens tidiga dagar—det är som ett säkerhetsnät. Kotlin erbjuder också `throw` för att manuellt kasta ett undantag in i ringen, och det finns `finally` för kod som måste köras—ofta städuppgifter.

Alternativ inkluderar `Result`-typen och Kotlins `try` som ett uttryck.

```Kotlin
val resultat: Result<Int> = try {
    Result.success(täljare / nämnare)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Detta tillvägagångssätt returnerar ett `Result`-objekt—du får antingen en framgång eller ett misslyckande utan dramatiken hos ett ohanterat undantag.

Implementeringen i Kotlin är snygg eftersom du kan använda `try` som ett uttryck, vilket innebär att det returnerar ett värde. Val som dessa gör felhanteringen i Kotlin ganska mångsidig. Det handlar om att välja rätt verktyg för jobbet, precis som du skulle göra i en verkstad.

## Se också
- Kotlin-dokumentation om Undantag: [Kotlin Exception Handling](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin-dokumentation om `Result`-typen: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3:e upplagan, av Joshua Bloch—fantastiska insikter om undantag, även om den är specifik för Java.
