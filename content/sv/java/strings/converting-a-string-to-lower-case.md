---
aliases:
- /sv/java/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:52.220637-07:00
description: "Att konvertera en str\xE4ng till gemener betyder att omvandla alla stora\
  \ bokst\xE4ver i texten till sm\xE5 bokst\xE4ver. Programmerare g\xF6r detta f\xF6\
  r att standardisera\u2026"
lastmod: 2024-02-18 23:08:51.657194
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener betyder att omvandla alla stora\
  \ bokst\xE4ver i texten till sm\xE5 bokst\xE4ver. Programmerare g\xF6r detta f\xF6\
  r att standardisera\u2026"
title: "Konvertera en str\xE4ng till gemener"
---

{{< edit_this_page >}}

## Vad och Varför?
Att konvertera en sträng till gemener betyder att omvandla alla stora bokstäver i texten till små bokstäver. Programmerare gör detta för att standardisera textdata, exempelvis vid jämförelser eller sökningar, där skillnader i versalisering inte ska påverka resultatet.

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
