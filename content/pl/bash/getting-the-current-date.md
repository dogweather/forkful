---
title:                "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć czytelnicy! W dzisiejszym wpisie będziemy rozmawiać o tym, dlaczego warto poznać sposoby pobierania bieżącej daty w języku Bash. Jest to podstawowa umiejętność, która może być bardzo przydatna podczas tworzenia skryptów i automatyzacji zadań.

## Jak To Zrobić

Aby pobrać aktualną datę w Bashu, wystarczy wykorzystać wbudowane funkcje systemowe. Jedną z nich jest polecenie `date`, które zwraca bieżącą datę i czas w ustawionym formacie. Oto kilka przykładowych zastosowań tego polecenia:

```
Bash
date +"%d/%m/%Y"    #wyświetli datę w formacie DD/MM/RRRR
date +"%H:%M:%S"    #wyświetli czas w formacie HH:MM:SS
date +"%A"          #wyświetli dzień tygodnia np. wtorek, środa
```

Wymienione powyżej przykłady to tylko niewielka część możliwości polecenia `date`. Istnieje wiele innych opcji, które pozwalają na bardziej wyszukane formatowanie daty i czasu. Możesz je znaleźć w dokumentacji polecenia lub po prostu spróbować eksperymentować z różnymi argumentami.

## Deep Dive

Poznajemy kolejną funkcję Bash o nazwie `date +%s`. Tym razem nie wyświetlimy formatowanej daty, a ilość sekund, która upłynęła od ery Unix'a (1 stycznia 1970 roku). Ciekawostką jest to, że wiele systemów operacyjnych przechowuje daty w takiej formie, co ułatwia porównywanie i obliczenia na dacie. Załóżmy, że potrzebujemy obliczyć, ile dni minęło od momentu, kiedy rozpoczęliśmy pracę nad naszym projektem. Wystarczy pobrać aktualną datę w sekundach i odjąć datę startową, a następnie podzielić wynik przez liczbę sekund w jednym dniu. Takie obliczenia są bardzo proste dzięki funkcjom Bash.

## Zobacz także

- Dokumentacja polecenia `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Poradnik o formatowaniu daty: https://devhints.io/bash-date-format
- Przykłady zastosowania funkcji `date` w codziennej pracy: https://www.cyberciti.biz/faq/linux-unix-get-yesterdays-tomorrows-date/