---
aliases:
- /sv/kotlin/rounding-numbers/
date: 2024-01-26 03:45:58.599488-07:00
description: "Att avrunda tal inneb\xE4r att justera dem till n\xE4rmaste hela tal\
  \ eller till en specifik precision. Programmerare g\xF6r det f\xF6r att f\xF6rb\xE4\
  ttra l\xE4sbarheten,\u2026"
lastmod: 2024-02-18 23:08:51.744876
model: gpt-4-0125-preview
summary: "Att avrunda tal inneb\xE4r att justera dem till n\xE4rmaste hela tal eller\
  \ till en specifik precision. Programmerare g\xF6r det f\xF6r att f\xF6rb\xE4ttra\
  \ l\xE4sbarheten,\u2026"
title: Avrundning av tal
---

{{< edit_this_page >}}

## Vad & Varför?

Att avrunda tal innebär att justera dem till närmaste hela tal eller till en specifik precision. Programmerare gör det för att förbättra läsbarheten, minska lagringskrav eller eftersom det exakta värdet inte är kritiskt för efterföljande beräkningar.

## Hur man gör:

I Kotlin kan avrundning göras med flera funktioner som `roundToInt()`, `roundToDouble()`, och med `BigDecimal` för mer kontroll:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Utskrift: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Utskrift: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Utskrift: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Utskrift: 123.5
}
```

## Fördjupning

Historiskt sett har avrundning av tal varit ett grundläggande koncept i både matematik och datavetenskap, utformad för att hantera begränsningar i numerisk precision. I tidig datorberäkning var avrundning avgörande på grund av det höga priset på minne.

I Kotlin bygger avrundning på standard Java-bibliotek. Alternativ för avrundning inkluderar `Math.round()`, som avrundar till närmaste hela tal, och `BigDecimal` för anpassningsbar avrundning, där du kan ange en skalning och ett `RoundingMode`.

Varje `RoundingMode` har olika policyer för att hantera oavgjorda situationer (när siffran är exakt i mitten av alternativen för avrundning). Till exempel rundar `RoundingMode.HALF_UP` till den närmaste grannen, såvida inte båda grannarna är lika avstånd från varandra, i vilket fall det avrundar uppåt.

## Se också

- Kotlin Dokumentation om [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracles Java Dokumentation för [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE-standarden för flyttalsaritmetik (IEEE 754) [IEEE Standard 754](https://ieeexplore.ieee.org/document/4610935)
