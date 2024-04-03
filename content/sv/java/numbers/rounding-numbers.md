---
date: 2024-01-26 03:45:27.213128-07:00
description: "Att avrunda tal inneb\xE4r att justera dem till en viss grad av noggrannhet.\
  \ Programmerare g\xF6r det f\xF6r att f\xF6renkla tal f\xF6r l\xE4sbarhet, f\xF6\
  r att uppfylla vissa\u2026"
lastmod: '2024-03-13T22:44:37.782465-06:00'
model: gpt-4-0125-preview
summary: "Att avrunda tal inneb\xE4r att justera dem till en viss grad av noggrannhet."
title: Avrundning av tal
weight: 13
---

## Hur man gör:
Java erbjuder flera sätt att avrunda tal. Här är en snabb demo med `Math.round()`, `BigDecimal`, och `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Använda Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Utdata: 123

        // Använda BigDecimal för mer kontroll
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Utdata: 123.46

        // Använda DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Utdata: 123.46
    }
}
```

## Fördjupning
Historiskt sett har avrundning av tal varit avgörande för analog beräkning och har följt med till digital databehandling för effektivitet och noggrannhet. Avrundningsfel, som de från flyttalsaritmetik, visar att detta inte är en trivial fråga - de kan ackumuleras och ställa till med beräkningar inom till exempel flyg- och finansapplikationer.

Bortom `Math.round()`, har du `BigDecimal`, som ger dig finare kontroll över skalan och avrundningsläget, och `DecimalFormat` när du behöver avrunda tal som en del av formatering av textutdata. Alternativ till avrundning inkluderar avkapning, avrundning uppåt och nedåt, som är olika sätt att hantera precision och hanteras typiskt av olika `Math`-metoder.

Beroende på ditt användningsfall kan avrundningsstrategin variera. Till exempel är `BigDecimal` att föredra för finansiella beräkningar, där precision är avgörande. I kontrast är `Math.round()` ett snabbt sätt för allmänna operationer där du är mindre petig med avrundningsläget.

## Se också
- [Oracles Java Math-dokumentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE-standarden för flyttalsaritmetik (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [DecimalFormat-klassen i Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
