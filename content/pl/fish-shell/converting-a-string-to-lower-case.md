---
title:    "Fish Shell: Konwertowanie ciągu znaków na małe litery"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja ciągów znaków na małe litery jest ważną umiejętnością dla każdego programisty. Używanie odpowiednich funkcji w językach programowania pozwala na łatwiejsze i bardziej skuteczne manipulowanie tekstem.

## Jak to zrobić

```Fish Shell``` ma kilka wbudowanych funkcji, które pozwalają na konwersję ciągów znaków na małe litery. Oto kilka przykładów, jak to zrobić:

```fish
string tolower "PROGRAMOWANIE" # zwraca "programowanie"
set tekst "To Jest Przykład" # zmienna z ciągiem znaków
echo $tekst | tolower # zwraca "to jest przykład"
```
Powyższe przykłady pokazują użycie funkcji ```tolower``` do konwersji ciągów znaków. Możliwe jest także zastosowanie tej funkcji w połączeniu z innymi, na przykład do czyszczenia i porównywania tekstu.

## Deep Dive

Funkcja ```tolower``` w ```Fish Shell``` jest częścią standardowej biblioteki shell i działa podobnie jak w innych językach programowania. Od słów kluczowych lub wrażliwości na wielkość liter zależy, jak dokładnie będzie działać funkcja.

Funkcja ```tolower``` jest także przydatna w przypadku wykorzystywania specjalnych znaków w tekstach, na przykład polskich liter lub znaków specjalnych.

## Zobacz także

- Dokumentacja funkcji ```tolower``` w ```Fish Shell```
- Poradnik o funkcji ```tolower``` w języku Python
- Przykładowy projekt wykorzystujący funkcję ```tolower``` do manipulowania tekstem