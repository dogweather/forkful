---
date: 2024-01-20 17:38:52.220637-07:00
description: "Hur man g\xF6r: **K\xF6rningsresultat:**."
lastmod: '2024-04-05T21:53:39.107983-06:00'
model: gpt-4-1106-preview
summary: "**K\xF6rningsresultat:**."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## Hur man gör:
```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Hej Världen!";
        String lowerCase = original.toLowerCase();

        System.out.println("Original: " + original);
        System.out.println("LowerCase: " + lowerCase);
    }
}
```
**Körningsresultat:**
```
Original: Hej Världen!
LowerCase: hej världen!
```

## Fördjupning
Historiskt har olika språk och kulturer haft olika regler för versalisering. I Java hanteras konverteringen till gemener genom `String`-klassens metoden `toLowerCase()`, vilken använder systemets standardlokaliseringsinställningar om inte annat anges. Alternativt kan `toLowerCase(Locale locale)`-metoden användas för att specificera en lokaliseringsinställning, vilket är viktigt för språk med andra versaliseringar än engelska. För att försäkra sig om ett korrekt beteende över olika system bör man lämpligen använda `Locale.ENGLISH` för engelsk text.

Implementationen av `toLowerCase()` kan variera mellan olika JDK-versioner (Java Development Kit), men syftar alltid till att följa Unicode-standardens rekommendationer för transformationsregler, vilket säkerställer konsistent beteende över olika språk och teckenuppsättningar.

## Se även
- [Java String toLowerCase() Method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Locale Class in Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- [Unicode Case Folding](http://www.unicode.org/reports/tr44/#CaseFolding)
