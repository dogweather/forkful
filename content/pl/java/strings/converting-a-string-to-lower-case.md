---
date: 2024-01-20 17:38:37.299020-07:00
description: "Konwersja stringa do ma\u0142ych liter to proces zmiany wszystkich liter\
  \ w tek\u015Bcie na ich ma\u0142e odpowiedniki. Robimy to dla jednolito\u015Bci\
  \ danych, \u0142atwiejszego\u2026"
lastmod: '2024-03-13T22:44:35.264149-06:00'
model: gpt-4-1106-preview
summary: "Konwersja stringa do ma\u0142ych liter to proces zmiany wszystkich liter\
  \ w tek\u015Bcie na ich ma\u0142e odpowiedniki."
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
