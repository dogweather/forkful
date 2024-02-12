---
title:                "Korzystanie z debugera"
aliases:
- /pl/swift/using-a-debugger/
date:                  2024-01-26T04:10:50.973196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Użycie debugera oznacza wykorzystanie specjalistycznych narzędzi do testowania i inspekcji twojego kodu podczas jego wykonania. To ważne, ponieważ pozwala zobaczyć, co dzieje się w tle, znaleźć błędy i lepiej zrozumieć zachowanie twojego kodu.

## Jak używać:
Aby użyć debugera w Xcode (IDE dla Swift), możesz ustawić punkty przerwania, inspektować zmienne i obserwować wyrażenia. Oto przykład:

```Swift
func znajdźSilnię(z liczby: Int) -> Int {
    if liczba == 0 {
        return 1
    }
    return liczba * znajdźSilnię(z: liczba - 1)
}

let wynik = znajdźSilnię(z: 5)
print(wynik)
```

Ustaw punkt przerwania, klikając po lewej stronie numeru linii w Xcode i uruchom program. Gdy trafi na punkt przerwania, Xcode zatrzyma wykonanie. Teraz możesz:

1. Sprawdzić wartości zmiennych.
2. Przejść do następnej linii (step over) lub wejść do środka funkcji (step into) za pomocą kontrolek debugera.
3. Dodać wyrażenia do "listy obserwowanych", aby monitorować zmiany w konkretnych zmiennych lub stałych.

Oto co możesz zobaczyć w obszarze debugowania:

```
(lldb) po liczba
5
(lldb) po wynik
120
```

## Pogłębione omówienie:
Debugery są częścią krajobrazu programistycznego od lat 40. XX wieku, ewoluując od prostych systemów przerwania do złożonych, sterowanych UI doświadczeń. Inne opcje poza wbudowanym debuggerem Xcode obejmują narzędzia stron trzecich, jak LLDB (Low Level Debugger), który Xcode wykorzystuje pod maską. Niektórzy debugują nawet używając instrukcji `print()` (nazywane czule "debugowaniem jaskiniowca"), ale jest to mniej efektywne dla dużych projektów lub złożonych błędów. Używając debugera, masz do czynienia z kontrolowaniem wykonania, introspekcją czasu wykonania i manipulacją danymi. Głębokie zrozumienie tych zasad znacznie ułatwia efektywne debugowanie.

## Zobacz również:
- [Przewodnik po debugowaniu Xcode od Apple](https://developer.apple.com/documentation/xcode/debugging/)
- [Szybki start LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Poradnik debugowania Swift od Ray'a Wenderlicha](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
