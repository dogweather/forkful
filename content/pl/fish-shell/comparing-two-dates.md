---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dat polega na ustaleniu, która z dwóch wprowadzonych dat jest wcześniejsza. Programiści stosują to do sortowania, filtrowania danych czy monitorowania różnych zdarzeń scenariuszy czasowych.

## Jak to zrobić:

```Fish Shell
# zaimportowanie modułu matematycznego
math (date -d "date1" +%s) - (date -d "date2" +%s)
```
Gdzie `date1` i `date2` to daty, które zamierzasz porównać. Przykładowe dane wyjściowe mogą wyglądać następująco:

```Fish Shell
86400  # Różnica w sekundach pomiędzy dwiema datami, w tym przypadku to jest 1 dzień.
```

## Wgłębnik: 

1. Kontekst historyczny: Wcześniejsze wersje Fish Shell wymagały bardziej skomplikowanych procesów do porównywania dat. Obecnie, dzięki integracji z modułem `math`, stało się to prostsze.
 
2. Alternatywy: Inne skrypty powłoki, takie jak Bash, zawierają również wbudowane funkcje do porównywania dat, ale Fish Shell oferuje bardziej proste rozwiązanie z modułem `math`.

3. Szczegóły implementacji: Powyższy kod działa, przekształcając daty na sekundy (od tzw. epoki Unix, czyli 1 stycznia 1970 roku), a następnie obliczając różnicę między nimi.

## Zobacz również:

Zapoznaj się z naszymi innymi zasobami dotyczącymi programowania w Fish Shell:

- Oficjalna dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
- Poradnik porównywania dat w innych skryptach powłoki: https://www.geekhideout.com/date.shtml
- Dodatkowe moduły dla Fish Shell: https://github.com/jorgebucaran/awesome-fish