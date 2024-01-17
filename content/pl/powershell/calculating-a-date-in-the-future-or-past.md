---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "PowerShell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości jest częstym zadaniem programistów, które polega na wyznaczeniu daty na podstawie podanej daty bazowej i określonego interwału czasowego. Jest to przydatne w tworzeniu narzędzi do planowania, obsługi danych czasowych lub generowania raportów.

# Jak to zrobić:
Obliczanie daty w przyszłości lub przeszłości jest bardzo proste w języku PowerShell. Wystarczy użyć polecenia ```Get-Date``` i określić pożądany interwał czasowy.

```PowerShell
# Oblicz datę dzisiejszą z dodanym tygodniem
Get-Date -Hour 0 -Minute 0 -Second 0 -Millisecond 0
$list[0].Created + [System.TimeSpan]::FromDays(7)
```

# Cztery najczęstsze zastosowania:
1. Obliczanie daty płatności lub terminów ważności dla faktur.
2. Generowanie raportów lub analizowanie danych czasowych.
3. Tworzenie narzędzi do planowania lub harmonogramowania.
4. Automatyzacja zadań związanych z datami, np. usuwanie starych plików.

# Głębsze spojrzenie:
1. Obliczanie daty w przeszłości lub przyszłości jest często wykorzystywane w biznesie i finansach od wielu lat.
2. Istnieją również inne sposoby obliczania dat, takie jak użycie bibliotek zewnętrznych lub zastosowanie języków specjalizujących się w manipulowaniu datami.
3. W języku PowerShell daty są przechowywane w postaci obiektów, dzięki czemu można wygodnie korzystać z różnych metod, takich jak dodawanie lub odejmowanie dni czy miesięcy.

# Zobacz również:
- [Dokumentacja języka PowerShell](https://docs.microsoft.com/pl-pl/powershell/)
- [Inne sposoby obliczania daty w języku PowerShell](https://sid-500.com/2018/09/15/lastlogon-powershell/)
- [Przykładowy skrypt obliczający datę w przyszłości lub przeszłości](https://www.experts-exchange.com/articles/30839/PowerShell-Adding-Days-to-Current-Date.html)