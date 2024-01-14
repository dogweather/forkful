---
title:                "Arduino: Konkatenacja ciągów znaków"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których programiści używają konkatynacji ciągów w Arduino. Jednym z głównych powodów jest możliwość łączenia różnych zmiennych i danych w jedną dłuższą wartość. Jest to przydatne w przypadku wyświetlania informacji lub tworzenia dynamicznych wiadomości.

## Jak to zrobić

Aby skonkatenować ciągi w Arduino, możesz użyć operatora "+" lub funkcji `strcat()`. Oto przykładowy kod:

```Arduino
// deklaracja zmiennych
String imie = "Maria";
String nazwisko = "Kowalska";
String pelneImie;

// użycie operatora "+"
pelneImie = imie + " " + nazwisko;

// wykorzystanie funkcji strcat()
strcat(pelneImie, " ");
strcat(pelneImie, nazwisko);

// wypisanie wyników
Serial.println(pelneImie); // wyświetli "Maria Kowalska"
```

W przypadku użycia operatora "+" zaleca się używanie typu danych `String`, a w przypadku funkcji `strcat()` - tablicy znaków. 

## Głębszy wgląd

Podczas konkatynacji ciągów, Arduino tworzy nowy ciąg przez połączenie istniejących ciągów. W przypadku użycia operatora "+", stosowane jest wygodne przeciążanie operatora, co pozwala na konkatynację różnych typów danych, nie tylko ciągów.

Natomiast funkcja `strcat()` jest nieco bardziej złożona, ponieważ wymaga podania adresu końcowego ciągu, do którego ma zostać dodany kolejny ciąg. Dlatego ważne jest, aby upewnić się, że ciąg, do którego jest dodawany kolejny ciąg, ma wystarczającą ilość pamięci zaalokowanej na potrzeby połączenia.

## Zobacz także

- Dokumentacja Arduino dotycząca konkatynacji ciągów: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strcat/
- Przykładowe projekty wykorzystujące konkatynację ciągów w Arduino: https://www.arduino.cc/search?q=String+concatenation