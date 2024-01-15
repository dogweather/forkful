---
title:                "Porównywanie dwóch dat"
html_title:           "Fish Shell: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu, szczególnie w pracy z większą ilością danych lub wydarzeń, konieczne jest porównanie dwóch dat. Jest to bardzo przydatne w celu wykrywania różnic w danych, analizy trendów lub określania czy dany wydarzenie wystąpiło wcześniej czy później. W tym artykule dowiesz się, jak porównywać dwie daty za pomocą Fish Shell, aktualnej wersji powłoki powłoki dla systemu macOS i Linux.

## Jak to zrobić

Aby porównać dwie daty za pomocą Fish Shell, musimy najpierw przypisać je do zmiennych. Możemy to zrobić za pomocą polecenia `set` wraz ze składnią `DATE = ..`. Na przykład:

```
Fish Shell  set DATE1 = 2021-01-01
Fish Shell  set DATE2 = 2021-01-31
```

Teraz, gdy mamy nasze daty przypisane do zmiennych, możemy je porównać. Istnieje kilka różnych sposobów porównywania dat w Fish Shell, które są dostępne dzięki wbudowanym funkcjom języka. Przedstawiamy dwa najczęściej używane:

- `abbrdate`: funkcja ta porównuje daty w formacie najpierw rok, potem miesiąc, a na końcu dzień. W przypadku naszych przykładowych dat, porównanie wyglądałoby następująco:

```
Fish Shell  abbrdate $DATE1 < abbrdate $DATE2
true
```

- `math`: funkcja ta umożliwia wykonywanie operacji matematycznych na datach. Oznacza to, że możemy np. odjąć jedną datę od drugiej, co pozwala nam na sprawdzenie, jak długi jest okres między nimi. Przykładowe porównanie:

```
Fish Shell  math $DATE2 - $DATE1
30
```

Po wykonaniu powyższych poleceń, otrzymujemy odpowiednie wartości `true` lub `30` (odpowiadające liczbie dni między datami). W przypadku, gdybyśmy chcieli sprawdzić czy jedna data jest równa drugiej, możemy również wykorzystać wbudowaną funkcję `eq` (equal).

## Głębsza analiza

Fish Shell oferuje także inne funkcje i operatory, które mogą być przydatne podczas porównywania dat. Na przykład funkcja `strptime` pozwala na konwersję daty w danym formacie na odpowiadającą jej wartość w unixowym timestamp. Dzięki temu możliwe jest porównywanie dat w różnych formatach (np. daty w formacie dd/mm/yyyy z datami w formacie yyyy/dd/mm).

Ponadto, Fish Shell umożliwia również korzystanie z wbudowanych zmiennych, takich jak `MY_DATE` lub `NOW`, które zawierają aktualną datę i godzinę. Można je wykorzystać w połączeniu z funkcjami opisanymi powyżej, aby wykonać bardziej zaawansowane operacje na datach.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o operacjach na datach w Fish Shell, możesz zapoznać się z oficjalną dokumentacją języka, dostępną na stronie https://fishshell.com/docs/current/index.html.

Jeśli interesuje Cię również programowanie w innych powłokach dla systemów macOS i Linux, polecamy artykuł https://www.leaseweb.com/labs/2019/04/powloki-w-systemach-unixowych-porownanie/.