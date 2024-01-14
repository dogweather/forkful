---
title:                "Java: Przekształcanie ciągu znaków na małe litery"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Dlaczego
Często w programowaniu potrzebujemy przekształcić tekst na małe litery. Jest to szczególnie przydatne w przypadku porównywania i przetwarzania danych. W tym artykule dowiesz się, jak możesz łatwo przekonwertować string na małe litery w języku Java.

# Jak to zrobić
```Java
String text = "KONWERTOWANIE TEKSTU NA MAŁE LITERY";
String lowercaseText = text.toLowerCase();
System.out.println(lowercaseText);
```
*Output:* konwertowanie tekstu na małe litery

Kod powyżej pokazuje przykładowe użycie metody `toLowerCase()`, która jest dostępna w klasie `String` w języku Java. Metoda ta konwertuje podany tekst na małe litery i zwraca nowy obiekt typu String.

Możesz również wykorzystać pętlę `for` i metodę `charAt()` w celu ręcznego konwertowania każdego znaku na małą literę:
```Java
String text = "KONWERTOWANIE TEKSTU NA MAŁE LITERY";
String lowercaseText = "";
for (int i = 0; i < text.length(); i++) {
    char c = text.charAt(i);
    if (Character.isUpperCase(c)) {
        c = Character.toLowerCase(c);
    }
    lowercaseText += c;
}
System.out.println(lowercaseText);
```
*Output:* konwertowanie tekstu na małe litery

# Deep Dive
W języku Java, konwersja tekstu na małe litery jest realizowana przez wykorzystanie standardowego wyrażenia regularnego `\p{javaLowerCase}`, które odpowiada wszystkim małym literom w alfabecie Unicode. Metoda `toLowerCase()` wykorzystuje to wyrażenie w celu wykonania konwersji.

W przypadku, gdy potrzebujemy konwertować tekst na małe litery w określonym języku, możemy wykorzystać metodę `toLowerCase(Locale)` i przekazać jako argument odpowiedni obiekt klasy `Locale`.

# Zobacz również
- [Dokumentacja Java - metoda toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Porównywanie tekstów - czy duże i małe litery mają znaczenie?](https://developer.mozilla.org/pl/docs/Web/JavaScript/Equality_comparisons_and_sameness#Z_eCMaScript_6_22806)