---
title:    "Java: Wyodrębnianie podciągów"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego warto zajmować się wyciąganiem podciągów?

W języku Java istnieje wiele przydatnych metod pozwalających operować na łańcuchach znaków. Jedną z nich jest metoda `substring()`, która umożliwia wyciąganie podciągów z danego łańcucha. Dlaczego więc warto się z nią zapoznać? Może Ci się przydać przy pracy z tekstem, gdy musisz wyciągnąć konkretną część wyrazu lub zdania. W tym artykule dowiesz się, w jaki sposób skutecznie używać metody `substring()`.

## Jak używać metody `substring()` w języku Java?

Metoda `substring()` jest częścią klasy `String` i służy do wyciągania określonego fragmentu łańcucha. Jej składnia wygląda następująco: `myString.substring(startIndex, endIndex)`, gdzie `startIndex` jest indeksem pierwszego znaku wyciąganego podciągu, a `endIndex` jest indeksem ostatniego znaku (nie wliczając go w podciąg). Poniżej znajduje się przykładowy kod w języku Java, który zilustruje to działanie:

```Java
String sentence = "Hello, World!";
String subString = sentence.substring(7, 12); // Wyciągnięcie podciągu od indeksu 7 do 11 (włącznie)
System.out.println(subString); // Output: World
```

W powyższym przykładzie zmienna `sentence` przechowuje pełne zdanie, a następnie przy użyciu metody `substring()` wyciągany jest podciąg od indeksu 7 do 11, czyli słowo "World". Dzięki temu rozwiązaniu możemy swobodnie manipulować tekstem i pobierać tylko te fragmenty, które nas interesują.

## Pogłębione informacje o wyciąganiu podciągów

Metoda `substring()` może przyjmować różne argumenty w zależności od naszych potrzeb. Jeśli podamy tylko jeden argument, np. `myString.substring(startIndex)`, to wyciągnie ona podciąg od danego indeksu do końca łańcucha. Możemy również pominąć argument `endIndex`, co spowoduje wyciągnięcie podciągu od danego indeksu do ostatniego znaku łańcucha.

Istnieje także możliwość użycia metody `substring()` w celu zamiany fragmentu łańcucha na inny. Wystarczy podać jako drugi argument nowy łańcuch, który zastąpi wyciągnięty fragment.

## Zobacz także

- Dokumentacja oficjalna metody `substring()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-
- Przykłady z wykorzystaniem `substring()` na stronie CodeJava.net: http://www.codejava.net/java-se/strings/how-to-extract-a-substring-from-a-string-in-java
- Wyciąganie podciągów w języku Java na przykładzie programu "Hello World": https://www.educba.com/substring-in-java/