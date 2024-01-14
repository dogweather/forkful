---
title:                "Arduino: Ekstrakcja podciągów."
simple_title:         "Ekstrakcja podciągów."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego wykorzystywać wyciąganie podłańcuchów?

W programowaniu dla Arduino często napotykamy sytuacje, w których potrzebujemy wyodrębnić pewną część tekstu lub liczby z większego ciągu znaków. Wtedy właśnie przydaje się umiejętność wyciągania podłańcuchów. Pozwala ona na łatwe i precyzyjne pobieranie tylko tych informacji, które są dla nas istotne. W tym artykule dowiesz się, jak dokonać takiego wyciągania przy użyciu języka Arduino.

## Jak to zrobić?

W celu wyciągnięcia podłańcucha z ciągu znaków w języku Arduino możemy skorzystać z funkcji ```substring()```. Przykładowe użycie tej funkcji wygląda następująco:

```arduino
String tekst = "Witaj, świecie!";
String podlacuch = tekst.substring(7, 14);
Serial.println(podlacuch);
```

Kod ten spowoduje wyświetlenie na monitorze szeregu znaków "świecie!", ponieważ wybrane zostały tylko znaki od 7 do 14 (licząc od zera). Możemy również wyciągnąć podłańcuch z końca ciągu znaków, podając tylko jedną liczbę do funkcji, np. ```substring(15)```. W takim przypadku zostaną wyświetlone znaki od podanej pozycji do końca tekstu.

## Deep Dive

Funkcja ```substring()``` może przyjąć dwa argumenty: początkową i końcową pozycję. Możemy również podać tylko jeden argument, a wtedy zostanie wybrany podłańcuch od podanej pozycji do końca ciągu znaków. Jest to bardzo przydatna funkcja, ponieważ pozwala na precyzyjne pobieranie interesujących nas informacji z tekstu. Pamiętajmy jednak, że indeksy znaków zawsze liczymy od zera, więc pierwsza pozycja tekstowa będzie miała indeks 0.

Możemy także użyć funkcji ```length()``` w celu sprawdzenia długości tekstu i wykorzystać to przy pobieraniu podłańcuchów. Np. jeśli chcemy wyciągnąć końcowe 10 znaków z tekstu, możemy napisać kod:

```arduino
String tekst = "Bardzo długi tekst";
int dlugosc = tekst.length();
String podlacuch = tekst.substring(dlugosc-10, dlugosc);
```

Tym razem jako argumenty funkcji podaliśmy długość tekstu pomniejszoną o 10, co spowoduje wybranie ostatnich 10 znaków.

# Zobacz także

- [Dokumentacja języka Arduino](https://www.arduino.cc/reference/en/language/functions/strings/substring/)
- [Tutorial o wyciąganiu podłańcuchów w języku C++](https://www.programiz.com/cpp-programming/library-function/cstring/substr)