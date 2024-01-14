---
title:    "Gleam: Konwertowanie daty na ciąg znaków"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków może być niezbędnym krokiem przy pracy z danymi w wielu aplikacjach. Na przykład, gdy tworzymy raporty lub wyświetlamy daty na stronie internetowej, zwykle potrzebujemy, aby daty były w czytelnej formie dla użytkowników. W tym artykule dowiecie się, jak w prosty sposób wykonac konwersję w języku programowania Gleam.

## Jak to zrobić

Aby skonwertować datę na ciąg znaków w języku Gleam, możemy skorzystać z wbudowanych funkcji formatujących w bibliotece standardowej. Najpierw musimy zaimportować moduł `Time`, który umożliwi nam pracę z datami. Następnie możemy wykorzystać funkcję `format`, aby skonfigurować formatowanie dla naszej daty. Poniżej znajduje się przykładowy kod, który konwertuje datę na ciąg znaków w formacie rok-miesiąc-dzień.

```Gleam
import Time

let date = Time.now()

let formatted_date = Time.format(date, "{year}-{month}-{day}")

![](https://i.imgur.com/pPKUJRT.png)
```

Jak widać powyżej, wynik konwersji jest przechowywany w zmiennej `formatted_date`. W ten sam sposób możemy wybrać inne formaty, np. miesiąc-dzień-rok lub dzień-miesiąc-rok. Jest to bardzo przydatne w przypadku tworzenia raportów lub etykiet z datami.

## Deep Dive

W języku Gleam istnieje wiele innych funkcji, które umożliwiają bardziej zaawansowane formatowanie dat. Możemy na przykład dodać informacje o dniu tygodnia lub użyć innych symboli dla formatowania. Wszystkie dostępne opcje znajdują się w [dokumentacji](https://gleam.run/modules/time) modułu `Time`.

## Zobacz także

- [Przewodnik po funkcjach biblioteki standardowej w języku Gleam](https://gleam.run/guides/stdlib.html)
- [Oficjalna strona języka Gleam](https://gleam.run/)
- [Przykładowe projekty w języku Gleam](https://github.com/gleam-lang)