---
title:                "Elm: Wypisywanie wyników debugowania"
simple_title:         "Wypisywanie wyników debugowania"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Rozwiązywanie problemów w kodzie może być frustrującym i czasochłonnym zadaniem. Dlatego warto nauczyć się korzystać z funkcji drukowania debugowego, aby lepiej zrozumieć, co dzieje się w naszym programie. Wykorzystanie drukowania debugowego jest niezwykle przydatne, zwłaszcza gdy pracujemy z językiem Elm.

## Jak to zrobić

Korzystanie z funkcji drukowania debugowego w języku Elm jest bardzo proste. Wystarczy napisać funkcję `Debug.log`, a następnie podać jej dwa argumenty: nazwę i wartość.

```Elm
Debug.log "Nazwa" wartosc
```

Bardzo ważne jest, aby wewnątrz funkcji `Debug.log` podać wyrażenie lub wartość, którą chcemy sprawdzić. W przeciwnym razie funkcja nie zadziała prawidłowo.

Aby zobaczyć działanie drukowania debugowego w akcji, przyjrzyjmy się poniższemu przykładowi:

```Elm
main =
  Debug.log "Nazwa" "Hello, World!"
  "Hello, World!"
```

Po uruchomieniu tego przykładu w przeglądarce, w konsoli deweloperskiej zobaczymy następujący wynik:

```
Nazwa :
  "Hello, World!"
Hello, World!
```

W pierwszej linijce zobaczymy nazwę, którą podaliśmy jako argument w funkcji `Debug.log`, a pod nią znajduje się wartość, którą chcieliśmy sprawdzić. W drugiej linijce zobaczymy zwracaną wartość przez funkcję `main`.

## Głębszy wgląd

Funkcja drukowania debugowego może być również użyta do drukowania informacji o typach zmiennych. Aby to zrobić, należy użyć funkcji `Debug.toString` wewnątrz funkcji `Debug.log`.

Na przykład, jeśli chcielibyśmy sprawdzić typ zmiennej liczbowej, napisalibyśmy:

```Elm
Debug.log "Typ zmiennej" (Debug.toString liczba)
```

W ten sposób w konsoli deweloperskiej zobaczymy typ zmiennej, który w tym przykładzie powinien być `number`.

## Zobacz również

- Elm debugowanie (https://elmprogramming.com/debugging-in-elm.html)
- Dokumentacja `Debug` w języku Elm (https://package.elm-lang.org/packages/elm/core/latest/Debug)