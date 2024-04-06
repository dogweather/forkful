---
date: 2024-01-20 17:38:37.299020-07:00
description: "Jak to zrobi\u0107: Metoda `toLowerCase()` w Java posiada d\u0142ug\u0105\
  \ histori\u0119 i jest cz\u0119\u015Bci\u0105 standardowej biblioteki od pocz\u0105\
  tk\xF3w j\u0119zyka. Opiera si\u0119 ona na regu\u0142ach\u2026"
lastmod: '2024-04-05T22:50:49.574570-06:00'
model: gpt-4-1106-preview
summary: "Metoda `toLowerCase()` w Java posiada d\u0142ug\u0105 histori\u0119 i jest\
  \ cz\u0119\u015Bci\u0105 standardowej biblioteki od pocz\u0105tk\xF3w j\u0119zyka."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## Jak to zrobić:
```java
public class LowerCaseConversion {

    public static void main(String[] args) {
        String originalText = "Jakiś Tekst z WIELKIMI i małymi Literami!";
        String lowerCaseText = originalText.toLowerCase();

        System.out.println("Original: " + originalText);
        System.out.println("LowerCase: " + lowerCaseText);
    }
}
```

Wyjście:
```
Original: Jakiś Tekst z WIELKIMI i małymi Literami!
LowerCase: jakiś tekst z wielkimi i małymi literami!
```

## Szczegółowe Informacje
Metoda `toLowerCase()` w Java posiada długą historię i jest częścią standardowej biblioteki od początków języka. Opiera się ona na regułach Unicode i może zachować się inaczej w zależności od lokalizacji (Locale). W wersji bezargumentowej używa domyślnej lokalizacji środowiska uruchomieniowego, która może nie być właściwa dla wszystkich języków.

Alternatywą jest uzycie przeciążonej wersji `toLowerCase(Locale locale)`, która pozwoli ci określić lokalizację i upewnić się, że konwersja obsłuży specyficzne przypadki, jak np. tureckie 'i', które ma inny odpowiednik małej litery niż w większości alfabetów.

```java
String originalText = "Istanbul";
System.out.println(originalText.toLowerCase(new Locale("tr", "TR"))); // wyjście: istanbul
System.out.println(originalText.toLowerCase()); // wyjście: istanbul lub ISTANBUL w zależności od domyślnej lokalizacji
```

Implementacja `toLowerCase()` używa wewnętrznych tabel Unicode, gdzie każdej dużej literze przypisuje się mały odpowiednik, uwzględniając różnice w poszczególnych językach.

## Zobacz też
- [Dokumentacja Oracle - metoda String.toLowerCase()](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Unicode Standard](https://www.unicode.org/standard/standard.html)
- [Java Internationalization: Understanding Locale](https://www.oracle.com/technical-resources/articles/javase/locale.html)
