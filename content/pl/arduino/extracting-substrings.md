---
title:                "Ekstrakcja podłańcuchów"
html_title:           "Arduino: Ekstrakcja podłańcuchów"
simple_title:         "Ekstrakcja podłańcuchów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego 

Wyodrębnianie podciągów może być przydatne w wielu projektach związanych z programowaniem na Arduino. Na przykład, możesz chcieć odczytać tylko część danych ze zewnętrznego czujnika lub wyświetlić fragment tekstu na wyświetlaczu LCD. Wykorzystanie wyodrębniania podciągów pozwoli Ci na łatwiejsze i bardziej precyzyjne manipulowanie danymi.

## Jak to zrobić

Wyodrębnianie podciągów jest możliwe dzięki funkcji ```substring()```. Należy podać dwa parametry: indeks początkowy i końcowy. Indeksy te odpowiadają pozycjom znaków w oryginalnym ciągu. Przykładowy kod może wyglądać następująco:

```
Arduino String example = "Lorem ipsum dolor sit amet";
String substring = example.substring(6,11);
Serial.println(substring);
```
W powyższym przykładzie wykorzystaliśmy ciąg znaków "Lorem ipsum dolor sit amet" oraz wyodrębniliśmy podciąg zaczynający się od 6. znaku (pierwszy znak ma indeks 0) i kończący na 11. znaku. Jego wynikiem będzie "ipsum".

Dodatkowo, funkcja ```substring()``` może być również wykorzystana w połączeniu z innymi funkcjami, takimi jak ```indexOf()```, aby znaleźć i wyodrębnić konkretne frazy lub słowa z ciągu znaków.

## Studium głębokie

Wyodrębnianie podciągów jest możliwe dzięki temu, że ciągi znaków są faktami niezmienialnymi (immutable) w języku Arduino. Oznacza to, że funkcja ```substring()``` nie zmienia oryginalnego ciągu, ale zwraca nowy ciąg.

Ponadto, funkcja ta może być wykorzystywana w połączeniu z innymi funkcjami dostępnymi w języku Arduino, takimi jak ```charAt()``` czy ```toCharArray()```, aby uzyskać jeszcze większą kontrolę nad manipulacją napisami.

## Zobacz także

- [Dokumentacja Arduino o funkcji substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Poradnik o manipulacji napisami na stronie Arduino Project Hub](https://create.arduino.cc/projecthub/The-dusty-corner/manipulating-strings-on-arduino-49ccb9)
- [Videotutorial na YouTube o wyodrębnianiu podciągów w języku Arduino](https://www.youtube.com/watch?v=qK8t4emft24)