---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Bash: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w życiu zdarzają się sytuacje, kiedy musisz wyliczyć datę w przyszłości lub przeszłości. Może to być potrzebne przy planowaniu wydarzeń lub przypomnieniach. W tym artykule dowiesz się, jak wykonać tę czynność za pomocą Bash.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, wykonaj następujące kroki:

1. Otwórz terminal i uruchom komendę `date`, aby wyświetlić aktualną datę i godzinę.
2. Następnie użyj flagi `-d` (ang. "date") i podaj żądany czas lub datę w formacie `MM/DD/YYYY`. Na przykład, jeśli chcesz obliczyć datę 7 dni w przyszłości, wpisz: `date -d "7 days"`.
3. Możesz również użyć flagi `-d` w połączeniu z opcją `+` i podać liczbę dni, miesięcy lub lat, które chcesz dodać lub odjąć. Na przykład, `date -d "+2 months"` spowoduje wyświetlenie daty dwie miesiące w przód.

Oto przykładowy kod, który wyświetli daty w przyszłości i przeszłości:

```Bash
# Wyświetla datę dzisiejszą
date

# Oblicza datę 10 dni w przyszłości
date -d "10 days"

# Oblicza datę 1 miesiąc w przód
date -d "+1 month"

# Oblicza datę 2 tygodnie w przeszłości
date -d "-2 weeks"
```

Te przykładowe komendy mogą być zmienione w zależności od potrzeb, aby uzyskać żądaną datę.

## Deep Dive

Funkcja obliczania daty w Bash korzysta z komendy `date`, która jest częścią programu GNU Coreutils. Umożliwia ona manipulację datami i czasami w różnych formatach.

Aby dodać lub odjąć dni, miesięcy lub lat, używamy formatu `%d` dla dni, `%m` dla miesięcy i `%Y` dla lat wraz z opcją `+` lub `-`, jak w przykładzie wcześniej. Możemy również wykorzystać bardziej szczegółowe formatowanie, takie jak `%H` dla godzin, `%M` dla minut i `%S` dla sekund.

Funkcja `date` obsługuje również formatowanie wyjścia. Dzięki temu możemy wyświetlać daty w różnych krajowych formatach, np. polskim czy amerykańskim. Możemy również zmienić wyświetlane informacje, takie jak godzina lub strefa czasowa, używając odpowiednich symboli formatujących.

## See Also

Jeśli chcesz dowiedzieć się więcej o funkcji `date` w Bash lub o formatowaniu wyjścia, zapoznaj się z dokumentacją programu [GNU Coreutils](https://www.gnu.org/software/coreutils/) lub wykonaj komendę `man date` w terminalu.