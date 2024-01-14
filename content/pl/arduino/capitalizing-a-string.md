---
title:    "Arduino: Zmiana wielkości liter w ciągu znaków"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Niezależnie od tego, czy jesteś początkującym czy doświadczonym programistą Arduino, z pewnością wszyscy już spotkaliśmy się z problemem zmiany wielkości liter w tekście. Może to być potrzebne w celu poprawnego wyświetlenia danych lub po prostu dla estetyki wyglądu. W tym wpisie dowiesz się, jak w prosty sposób zastosować funkcję do zmiany wielkości liter na platformie Arduino.

## Jak To Zrobić
Do zmiany wielkości liter w tekście na platformie Arduino można użyć wbudowanej funkcji `toUpperCase()`. Należy jednak pamiętać, że funkcja ta działa tylko dla znaków ASCII i dotyczy tylko liter łacińskich. Poniżej przedstawiony jest przykład kodu, który używa tej funkcji:

```Arduino
String napis = "Cześć świat!";
String napis_wielkie_litery = napis.toUpperCase();
```

Powyższy kod przypisuje zmiennej `napis_wielkie_litery` wartość napisu `CZEŚĆ ŚWIAT!`, ponieważ funkcja `toUpperCase()` zamienia wszystkie litery na wielkie. Można również wyświetlić ten napis na monitorze szeregowym Arduino, używając funkcji `Serial.println()`:

```Arduino
Serial.println(napis_wielkie_litery);
```

Output: `CZEŚĆ ŚWIAT!`

## Głębszy Rzut Oka
W przypadku, gdy używasz znaków spoza ASCII lub innych systemów kodowania, istnieją inne sposoby na zmianę wielkości liter. Możesz na przykład utworzyć tablicę znaków i używać pętli `for` w celu zmiany wielkości poszczególnych znaków. Możesz również poszukać gotowych bibliotek, które pomogą w szybszej i bardziej zaawansowanej zmianie wielkości liter.

## Zobacz Również
- [Funkcja toUpperCase() na stronie Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [Biblioteka TextReader](https://www.arduinolibraries.info/libraries/text-reader)
- [ASCII – Wikipedia](https://pl.wikipedia.org/wiki/ASCII)