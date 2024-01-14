---
title:    "Gleam: Znajdowanie długości ciągu znaków"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie programowania musimy wyznaczyć długość ciągu znaków. Jest to istotny element w wielu aplikacjach i niezbędny w wielu przypadkach, dlatego warto nauczyć się, jak w łatwy sposób znaleźć długość ciągu przy użyciu języka Gleam.

## Jak to zrobić

Aby znaleźć długość ciągu w języku Gleam, możemy skorzystać z funkcji `String.length/1`. Przyjmuje ona jako argument ciąg znaków, którego długość chcemy znaleźć. Poniżej znajduje się przykładowy kod z użyciem tej funkcji:

```Gleam
let str = "To jest przykładowy ciąg znaków"
let length = String.length(str)
```

W powyższym przykładzie zmienna `length` będzie przechowywać długość ciągu, czyli w tym przypadku wartość 32. Aby wyświetlić wynik, możemy skorzystać z funkcji `Debug.show/1`:

```Gleam
let str = "To jest przykładowy ciąg znaków"
let length = String.length(str)
Debug.show(length)
```

Po uruchomieniu tego kodu w konsoli otrzymamy wynik `32`.

## Deep Dive

Warto zauważyć, że funkcja `String.length/1` zwraca liczbę bajtów w ciągu, a nie liczbę znaków. W przypadku języków z alfabetem ASCII (np. angielski) nie ma to większego znaczenia, ponieważ jeden znak to jeden bajt. Jednak w przypadku języków z alfabetami rozszerzonymi, może to wpłynąć na otrzymany wynik. Na przykład, w języku polskim litera `ą` zajmuje dwa bajty, więc ciąg "ąą" będzie miał długość 4, pomimo że składa się tylko z dwóch liter.

## Zobacz także

- Dokumentacja języka Gleam: https://gleam.run/
- Funkcja String.length/1 w dokumentacji: https://gleam.run/std.html#String.length-function

Dzięki tym prostym przykładom możemy łatwo i szybko znaleźć długość ciągu znaków w języku Gleam. Zastosowanie tej funkcji może okazać się bardzo przydatne w wielu projektach. Zapraszamy do kontynuowania nauki języka Gleam i odkrywania jego możliwości.