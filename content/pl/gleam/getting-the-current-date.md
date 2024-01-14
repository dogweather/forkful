---
title:                "Gleam: Pobieranie bieżącej daty"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy poznać aktualną datę? W programowaniu, często musimy używać czasu aby wyświetlić bieżące wydarzenia, wykonać operacje na plikach lub określić ważne daty w projekcie. Dzięki temu, poznanie aktualnej daty jest niezwykle ważne i przydatne w wielu sytuacjach.

## Jak to zrobić

Aby uzyskać aktualną datę w Gleam, możemy wykorzystać wbudowaną funkcję `Date.now()`. Przykładowy kod i wynik można zapisać w bloku kodu Gleam:

```
gleam -module date_example

pub fn main() {
  let current_date = Date.now()
  IO.println("Aktualna data: " ++ current_date)
}
```

Wyjście powinno wyglądać mniej więcej tak:

```
Aktualna data: 2020-05-08T19:56:23.233Z
```

## Wnikliwe spojrzenie

Jeśli chcemy wiedzieć więcej na temat wykorzystania daty w Gleam, możemy przejrzeć dokumentację języka, która dostępna jest na oficjalnej stronie Gleam. Istnieje również wiele bibliotek zewnętrznych, takich jak `calendar` czy `datetime`, które mogą pomóc w manipulowaniu danymi i przekształcaniu ich w różne formaty. 

## Zobacz również

- [Dokumentacja Gleam](https://gleam.run/)
- [Biblioteka calendar dla Gleam](https://github.com/gleam-lang/calendar)
- [Biblioteka datetime dla Gleam](https://github.com/philippneugebauer/gleam-datetime)