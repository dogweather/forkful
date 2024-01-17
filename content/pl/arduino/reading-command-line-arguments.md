---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Arduino: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czy kiedykolwiek potrzebowałeś/musiałaś wywołać jakąś funkcję, przekazując do niej pewne parametry lub opcje? Wielu programistów z pewnością natknęło się na taką konieczność. Dzięki czytaniu argumentów z linii poleceń możemy dostosować funkcjonalność naszych programów w czasie wykonania. Jest to niezwykle przydatne narzędzie, które umożliwia nam dynamiczne wywoływanie funkcji w zależności od danych, z jakimi uruchamiany jest program.

## Jak to zrobić:

Arduino pozwala na odczytywanie argumentów z linii poleceń dzięki funckji ```Arduino.readArguments()```. Aby użyć tej funkcji, musimy najpierw dostarczyć jej odpowiedni obiekt, który przechowuje argumenty. Przykład dostarczania obiektu:
```
Arduino arduino = new Arduino();

int main() {
  arduino.readArguments();
  // kod realizujący dostarczone parametry
}
```

Po wywołaniu tej funkcji, możemy odczytywać argumenty za pomocą metody ```readArguments.getArgument()```. Przykładowy kod czytania argumentów i ich wykorzystania:
```
int argument1 = arduino.readArguments().getArgument(0);
int argument2 = arduino.readArguments().getArgument(1);

if(argument1 == 0){
  // wywołanie pewnej funkcji
}

if(argument2 == 1){
  // wywołanie innej funkcji
}
```

## Zagłębienie:

Czytanie argumentów z linii poleceń jest bardzo popularną praktyką w programowaniu od wielu lat. Zostało wprowadzone w celu umożliwienia programistom dostosowywania funkcjonalności swoich programów bez konieczności zmieniania kodu źródłowego. Alternatywą dla czytania argumentów z linii poleceń jest tworzenie konfiguracyjnych plików, które są odczytywane przez program podczas jego uruchamiania.

Implementacja czytania argumentów z linii poleceń może różnić się w zależności od platformy i języka programowania. W Arduino, funkcjonalność ta jest dostępna dzięki bibliotece commandlineargs, która musi być zainstalowana w celu jej użycia.

## Zobacz także:

Aby dowiedzieć się więcej o czytaniu argumentów z linii poleceń w Arduino, możesz przeczytać dokumentację biblioteki commandlineargs dostępnej [tutaj](https://github.com/kroimon/ArduinoCommandLineArgs).

Jeśli szukasz alternatywy dla czytania argumentów z linii poleceń, warto przyjrzeć się tworzeniu plików konfiguracyjnych, które mogą być odczytywane przez program przy jego uruchamianiu.