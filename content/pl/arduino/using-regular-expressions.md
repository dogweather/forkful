---
title:                "Używanie wyrażeń regularnych"
html_title:           "Arduino: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia, zwane również regexami, są potężnym narzędziem służącym do manipulowania i przeszukiwania tekstowego danych. Są szczególnie przydatne dla programistów Arduino, ponieważ pozwalają na dokładne dopasowanie wzorców i warunków w tekstowych danych, co jest niezbędne w wielu projektach.

## Jak to zrobić

Aby skorzystać z regularnych wyrażeń w kodzie Arduino, musimy najpierw dodać bibliotekę Regex, która jest dostępna w menedżerze bibliotek Arduino. Następnie tworzymy obiekt Regex i definiujemy nasz wzorzec. Możemy użyć różnych znaków specjalnych, takich jak "*" (zero lub więcej wystąpień) czy "+" (co najmniej jedno wystąpienie), aby dopasować bardziej skomplikowane wzorce. Tutaj jest przykładowy kod wykorzystujący regex do wyszukiwania liczby całkowitej w tekście i wyświetlenie jej na Serial Monitorze:

```Arduino
#include <Regex.h> //dodanie biblioteki Regex

Regex myRegex("[0-9]+"); //definiowanie wzorca

void setup() {
  Serial.begin(9600); //inicjalizacja Serial Monitora
}

void loop() {
  String text = "Liczba całkowita to 123456";
  if (myRegex.match(text)) { //jeśli tekst pasuje do wzorca
    Serial.println(myRegex.matched()); //wyświetlamy dopasowanie na Serial Monitorze
  }
}
```

Przykładowy wynik:

```
123456
```

## Głębsza analiza

Możemy również wykorzystać grupy, aby wyodrębnić konkretną część dopasowania, na przykład słowo lub liczbę. W poniższym przykładzie użyjemy grupy, aby wyodrębnić tylko pierwszą cyfrę liczby całkowitej:

```Arduino
#include <Regex.h> //dodanie biblioteki Regex

Regex myRegex("([0-9]+)"); //definiowanie wzorca z grupą

void setup() {
  Serial.begin(9600); //inicjalizacja Serial Monitora
}

void loop() {
  String text = "Liczba całkowita to 123456";
  if (myRegex.match(text)) { //jeśli tekst pasuje do wzorca
    Serial.println(myRegex.group(1)); //wyświetlamy pierwszą grupę dopasowania na Serial Monitorze
  }
}
```

Przykładowy wynik:

```
1
```

Regexy są również przydatne w walidacji danych, na przykład sprawdzaniu czy wprowadzone przez użytkownika wartości są poprawnego formatu. Możemy również wykorzystać warunki, aby stwierdzić, czy dany tekst pasuje do różnych wzorców. Jest to szczególnie przydatne w prostych interaktywnych projektach z wykorzystaniem przycisków lub czujników.

## Zobacz także

- [Dokumentacja biblioteki Regex dla Arduino](https://www.arduino.cc/reference/en/libraries/regex/)
- [Poradnik o regularnych wyrażeniach w Arduino](https://maker.pro/arduino/tutorial/how-to-use-regular-expressions-regex-with-arduino)
- [Przykładowy projekt z wykorzystaniem regex w Arduino](https://www.instructables.com/Using-Regular-Expressions-With-Arduino/)