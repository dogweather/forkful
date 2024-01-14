---
title:    "Bash: Uzyskiwanie bieżącej daty"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie bieżącej daty może być bardzo przydatne podczas pisania skryptów lub programów z użyciem języka Bash. Pozwala to na automatyczne wykrycie aktualnego dnia, tygodnia lub miesiąca, co ułatwia wykonywanie różnych działań programistycznych.

## Jak to zrobić

Poniżej przedstawione są przykładowe kody w Bash, które pozwolą na pobranie bieżącej daty. Wszystkie przykłady zostały przedstawione w formie bloków kodu, aby ułatwić zrozumienie.

```Bash
# Pobranie bieżącego dnia
echo $(date +%d)

# Pobranie bieżącego tygodnia
echo $(date +%w)

# Pobranie bieżącego miesiąca
echo $(date +%m)

# Pobranie bieżącego roku
echo $(date +%Y)
```

Powyższe przykłady korzystają z wbudowanej w języku Bash komendy `date`, która pozwala na manipulację datami. Warto zauważyć, że w każdym przykładzie używamy znaku procentu `%` przed literą oznaczającą dany element daty. Ponadto, można również zmienić format wyświetlania daty przez dodanie odpowiednich znaków lub słów kluczowych wewnątrz nawiasów procentowych.

## Dogłębny przegląd

W języku Bash, aby pobrać bieżącą datę, używa się komendy `date`. Jest to jeden z wielu wbudowanych poleceń, które pozwalają na manipulację i wyświetlanie dat w różnych formatach. Oprócz podstawowych poleceń przedstawionych powyżej, `date` oferuje wiele innych opcji, takich jak wyświetlanie daty w różnych strefach czasowych, operacje na datach, czy nawet wyświetlanie kalendarza. Aby poznać wszystkie możliwości tej komendy, warto przejrzeć jej dokumentację lub skorzystać z opcji `man` w terminalu.

## Zobacz również

- [Dokumentacja komendy `date`](https://ss64.com/bash/date.html)
- [Kalendarz Języka Bash](https://wiki.bash-hackers.org/commands/builtin/date)