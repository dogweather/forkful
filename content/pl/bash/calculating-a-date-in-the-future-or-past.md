---
title:                "Bash: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym świecie programowanie to nie tylko narzędzie dla specjalistów, ale także przydatna umiejętność dla każdego. Jednym z ważnych aspektów programowania jest możliwość manipulacji datami. Czasem musimy obliczyć datę w przyszłości lub w przeszłości na potrzeby naszej aplikacji lub projektu. W tym artykule dowiesz się jak w łatwy sposób obliczyć datę w języku Bash.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Bash, należy użyć komendy "date" wraz z odpowiednimi argumentami. Przykładowo, aby obliczyć datę za 5 dni:

```Bash
date -d "+5 days"
```

To samo możemy zrobić dla daty w przeszłości, dodając myślnik przed liczbą dni. Przykładowo, aby obliczyć datę sprzed 5 dni:

```Bash
date -d "-5 days"
```

Możemy także określić konkretny dzień, miesiąc lub rok. Należy wtedy użyć argumentów "-d" oraz "-yyyy" dla roku, "-mm" dla miesiąca i "-dd" dla dnia. Na przykład, aby obliczyć datę za 3 lata i 2 miesiące:

```Bash
date -d "3 year 2 month"
```
Możemy także używać różnych jednostek czasu, takich jak tygodnie, godziny, minuty, etc. Pełną listę argumentów i ich możliwości można znaleźć w dokumentacji komendy "date".

## Głębszy wgląd

W języku Bash możemy także użyć komendy "date" z argumentem "-s" do ustawienia konkretnej daty. Na przykład:

```Bash
date -s "2020-05-05 12:00:00"
```
Spowoduje ustawienie aktualnej daty na 5 maja 2020 roku o godzinie 12:00:00.

Jednak warto pamiętać, że komenda "date" korzysta z ustawień systemu operacyjnego, więc zmiana daty może być ograniczona przez jego ustawienia.

## Zobacz także

- [Dokumentacja komendy date](http://www.tutorialspoint.com/unix_commands/date.htm)
- [Inne przydatne komendy w języku Bash](https://kasia.codes/10-useful-bash-commands/)