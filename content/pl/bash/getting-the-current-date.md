---
title:    "Bash: Pobieranie aktualnej daty"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele osób angażuje się w programowanie z użyciem języka Bash. Jest to bardzo wydajny i łatwy w użyciu język skryptowy, który może pomóc w wykonywaniu wielu zadań. Jednym z przydatnych zadań, które można wykonać z użyciem Bash, jest pobieranie aktualnej daty. W tym artykule przyprowadzę Cię przez proces pobierania aktualnej daty za pomocą Bash.

## Jak to zrobić

Aby pobrać aktualną datę za pomocą Bash, musimy wpisać następującą komendę w terminalu:

```Bash
date
```

Po jej wykonaniu, w terminalu pojawi się aktualna data w formcie: dzień tygodnia, dzień miesiąca, miesiąc oraz rok. Na przykład: "środa, 15 września 2021".

Możemy również użyć opcji -u, aby uzyskać uniwersalną datę i godzinę.

```Bash
date -u
```

Wynik będzie wyglądał mniej więcej tak: "sobota, 18 września 2021, 20:27:42 UTC".

Jeśli chcemy wyświetlić tylko konkretną część daty, możemy użyć opcji -d wraz z odpowiednim formatem. Na przykład, jeśli chcemy wyświetlić tylko dzień miesiąca:

```Bash
date -d "%d"
```

Wynik będzie wyglądał na przykład tak: "18".

Inną przydatną opcją jest -f, która pozwala nam wprowadzić własny format daty. Na przykład, jeśli chcemy wyświetlić datę w formacie "RRRR-MM-DD":

```Bash
date -f "%Y-%m-%d"
```

Wynik będzie wyglądał na przykład tak: "2021-09-18".

## Deep Dive

Bash korzysta z funkcji date, która jest częścią coreutils. Wywołanie tej funkcji powoduje uruchomienie programu date. Program ten jest odpowiedzialny za pobieranie aktualnej daty z systemu operacyjnego i zwracanie jej w odpowiednim formacie. Używając opcji -u, możemy uzyskać datę w formacie UTC, który jest standardowym formatem czasu dla wielu systemów operacyjnych.

Warto również wspomnieć, że program date może dzięki swojej funkcji parsowania, konwertować datę w różne formaty, co czyni go niezwykle przydatnym narzędziem w programowaniu.

## Zobacz też

- [Dokumentacja Coreutils - date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Bash Guide for Beginners - Working with Dates and Times](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_10_02.html)