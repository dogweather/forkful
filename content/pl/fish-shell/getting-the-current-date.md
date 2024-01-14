---
title:    "Fish Shell: Pobieranie aktualnej daty"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie jest nie tylko o tworzeniu nowych funkcjonalności, ale także o wykorzystaniu narzędzi, które ułatwiają pracę i oszczędzają czas. Jednym z takich narzędzi jest zdolność do pobierania aktualnej daty wskazującej na bieżący czas. W tym artykule dowiesz się, jak to zrobić za pomocą powłoki Fish Shell.

## Jak to zrobić

Aby pobrać aktualną datę w Fish Shell, użyj polecenia `date` z odpowiednimi flagami. Oto kilka przykładów:

```Fish Shell
date +%d/%m/%Y     # Wyświetli datę w formacie dzień/miesiąc/rok
>> 14/04/2021

date +%A            # Wyświetli bieżący dzień tygodnia
>> Środa

date +%T            # Wyświetli aktualny czas w formacie godzina:minuty:sekundy
>> 16:32:49
```
Możesz użyć różnych kombinacji flag, aby uzyskać pożądany format daty i czasu. Szczegółowe informacje o dostępnych flagach można znaleźć w dokumentacji Fish Shell.

## Wnikliwa analiza

Polecenie `date` jest częścią standardowych narzędzi systemowych i działa na wszystkich systemach operacyjnych, które obsługują powłokę Fish. W przypadku, gdy chcesz użyć innej strefy czasowej niż domyślna, możesz użyć flagi `-u` oraz podać odpowiedni kod dla danej strefy. Na przykład, `-u EST` dla strefy Eastern Standart Time.

Ponadto, polecenie `date` może również przyjmować opcję `-d`, która pozwala na obliczenie daty w przyszłości lub w przeszłości. Na przykład, `date +"%d/%m/%Y" -d "2 days ago"` wyświetli datę sprzed dwóch dni.

## Zobacz również

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Dokumentacja polecenia `date`](https://www.unix.com/man-page/linux/1/date/)
- [Porównanie powłok: Bash vs. Fish](https://dev.to/manan30/bash-vs-fish-which-is-the-shell-for-you-9c4)