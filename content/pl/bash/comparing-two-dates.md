---
title:                "Bash: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego
Porównywanie dat może wydawać się prostym zadaniem, ale może być bardzo przydatne w programowaniu. Jest to nie tylko jedna z podstawowych funkcji w Bash, ale również przydatna w wielu innych językach programowania. Pozwala nam na ustalenie, czy jedna data jest wcześniejsza, późniejsza lub równa drugiej dacie. W tym artykule dowiemy się, jak porównywać daty w Bash i jakie są najczęstsze zastosowania tego narzędzia.

## Jak to zrobić
Porównywanie dat w Bash jest bardzo proste. Wystarczy wykorzystać operator porównania `[[ ... ]]` oraz opcję `<` lub `>` w połączeniu z poleceniem `test`. Przykładowe użycie wyglądałoby następująco:

```Bash
if [[ "2020-01-01" < "2020-02-01" ]]; then
    echo "Pierwsza data jest wcześniejsza od drugiej"
fi
```

W powyższym kodzie porównujemy dwie daty i w zależności od wyniku, wyświetlamy odpowiedni komunikat. Pamiętajmy, że porównanie działa tylko wtedy, gdy obie daty są w formacie `YYYY-MM-DD`.

Możemy również porównywać daty z bieżącą datą lub dzisiejszą datą. Wykorzystujemy do tego polecenie `date` wraz z opcją `%F` do uzyskania daty w formacie `YYYY-MM-DD`. Przykładowy kod wyglądałby następująco:

```Bash
if [[ $(date +%F) > "2020-12-31" ]]; then
    echo "Bieżąca data jest późniejsza od podanej daty"
fi
```
W powyższym przykładzie, jeśli dzisiejsza data jest późniejsza od 31.12.2020, pojawi się odpowiedni komunikat.

## Deep Dive
Aby bardziej zgłębić temat porównywania dat, warto wiedzieć, że w Bash możemy też określać różnicę między dwoma datami. W tym celu możemy wykorzystać polecenie `date` wraz z opcją `%s`, które zwraca liczbę sekund od 1 stycznia 1970 roku. Przykładowy kod wyglądałby tak:

```Bash
first_date=$(date -d "2020-01-01" +%s)
second_date=$(date -d "2020-02-01" +%s)

echo $(( second_date - first_date ))
```
Powinniśmy otrzymać wynik równy 2678400, co odpowiada liczbie sekund w miesiącu styczeń. Możemy również porównywać różnice w liczbach sekund między datami, a nie same daty.

## Zobacz również
- [Dokumentacja Bash o porównywaniu liczb i ciągów tekstowych](https://tldp.org/LDP/abs/html/comparison-ops.html)
- [Porównywanie dat w Python](https://realpython.com/python-datetime/)
- [Porównywanie dat w C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)