---
title:                "Konwertowanie tekstu na wielkie litery"
html_title:           "Arduino: Konwertowanie tekstu na wielkie litery"
simple_title:         "Konwertowanie tekstu na wielkie litery"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu spotykamy się z koniecznością modyfikacji tekstu w taki sposób, aby każde słowo rozpoczynało się wielką literą. W tym artykule dowiesz się, jak łatwo i szybko zastosować ten zabieg w swoim kodzie.

## Jak to zrobić

### Przykład kodu

```Arduino
String input = "programowanie jest świetną umiejętnością";
String output = "";

// Pętla, która przejdzie przez każde słowo
for (int i = 0; i < input.length(); i++) {

    // Sprawdzamy, czy dany znak jest literą
    if (isAlpha(input.charAt(i))) {

        // Jeśli tak, to zamieniamy na wielką literę i dodajemy do wyjściowego stringa
        output += char(toupper(input.charAt(i)));
    }
    else {
        // Jeśli nie jest literą, dodajemy znak bez zmian
        output += input.charAt(i);
    }
}

// Wyświetlamy wynik
Serial.println(output);
```

### Wynik

Kod powyżej pozwala na zamianę tekstu "programowanie jest świetną umiejętnością" na "Programowanie Jest Świetną Umiejętnością".

## Co poza podstawami?

Jeśli chcesz przejść jeszcze głębszą analizę tematu, warto zwrócić uwagę na różne sposoby manipulacji tekstem. Jednym z nich jest wykorzystanie biblioteki <i>string.h</i>, która posiada wbudowane funkcje umożliwiające m.in. zamianę wszystkich liter na duże lub małe, a także odwrócenie kolejności liter w słowie. Warto również zapoznać się z działaniem pętli oraz funkcjami <i>charAt()</i> i <i>toupper()</i>, które są niezbędne do wykonania operacji na pojedynczych znakach.

## Zobacz również

- [Arduino - Oficjalna strona](https://www.arduino.cc/)
- [Podstawy języka C++ dla Arduino](https://create.arduino.cc/projecthub/muhammad-aqib/arduino-basics-for-beginners-4dfa26)
- [Manipulacja tekstem z użyciem biblioteki string.h](https://www.programmingelectronics.com/arduino-string-function-examples/)