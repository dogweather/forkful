---
title:                "Pobieranie bieżącej daty"
html_title:           "Gleam: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie aktualnej daty jest jednym ze sposobów na uzyskanie informacji o bieżącym czasie. Programiści często wykonują tę operację w swoim kodzie, ponieważ jest to niezbędne dla wielu aplikacji, takich jak kalendarze, alarmy, czy systemy rejestracji czasu pracy.

## Jak to zrobić?

To bardzo proste! Wystarczy użyć wbudowanej funkcji `Date.now` w języku programowania Gleam. Poniżej znajdują się przykłady kodu i wyników dla różnych formatów daty.

```Gleam 
// Pobierz datę w formacie rok-miesiąc-dzień
let bieżąca_data = Date.now("%Y-%m-%d")
``` 
```Gleam
// Wynik: 2021-10-14
```

```Gleam
// Pobierz datę w formacie dzień/miesiąc/rok
let bieżąca_data = Date.now("%d/%m/%Y")
``` 
```Gleam 
// Wynik: 14/10/2021
```

```Gleam
// Pobierz godzinę w formacie 24-godzinnym
let bieżąca_data = Date.now("%H:%M:%S")
```
```Gleam 
// Wynik: 15:30:12
```

## Wgląd w szczegóły

Pobieranie aktualnej daty jest czynnością, którą programiści wykonują od lat, ponieważ jest ona niezbędna do wielu funkcji w aplikacjach. Jednym z alternatywnych sposobów na uzyskanie aktualnej daty jest użycie zewnętrznej biblioteki, jednak wbudowane w język funkcje jak `Date.now` są szybsze i nie wymagają instalacji dodatkowego kodu.

W języku Gleam, pobieranie aktualnej daty jest możliwe dzięki wykorzystaniu standardowej biblioteki czasu (`Time`), która jest zbudowana na języku Erlang. Wewnątrz tej biblioteki istnieje funkcja `now` odpowiadająca za pobieranie bieżącego czasu.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o formatowaniu dat w języku Gleam, zapoznaj się z dokumentacją na oficjalnej stronie: https://gleam.run/documentation/std-lib/time

Możesz także skorzystać z innych funkcji dostępnych w bibliotece czasu, takich jak obliczanie różnicy czasu między dwoma datami. Od teraz pobieranie aktualnej daty nie będzie już dla Ciebie tajemnicą!