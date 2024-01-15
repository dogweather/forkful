---
title:                "Pisanie testów."
html_title:           "Bash: Pisanie testów."
simple_title:         "Pisanie testów."
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego powinieneś pisać testy w Bashu? Testowanie jest nieodłączną częścią pisania kodu, a w Bashu jest to szczególnie ważne. Dzięki testom możesz mieć pewność, że twoje aplikacje będą działać zgodnie z oczekiwaniami, unikając nieprzewidzianych błędów.

## Jak To Zrobić

Pisanie testów w Bashu jest bardzo proste i nie wymaga specjalistycznej wiedzy. Wystarczy najpierw napisać skrypt, który będzie testował określone funkcje lub zachowania, a następnie uruchomić go w terminalu. Poniżej znajdziesz przykładowy kod testu oraz jego wynik.

 ```Bash
#!/bin/bash

# Przykładowy test sprawdzający czy podana liczba jest parzysta
result=$(./test.sh 4)

if [ "$result" = "liczba parzysta" ]; then
  echo "Test Passed"
else
  echo "Test Failed"
fi

```


 ```Bash
Test Passed
```

## Głębszy Wgląd

Istnieje wiele narzędzi, które mogą pomóc Ci w pisaniu testów w Bashu, takich jak np. framework Bats (Bash Automated Testing System) lub wbudowany w Bash funkcjonalność `test`. Ważne jest również, aby pisać testy w taki sposób, aby były przenośne i działały na różnych systemach operacyjnych oraz w różnych środowiskach.

## Zobacz Również

- [Dokumentacja Bats](https://github.com/sstephenson/bats)
- [Wprowadzenie do testów w Bashu](https://opensource.com/article/18/9/introduction-bash-testing)
- [Porządki z testami w Bashu](https://www.jeffgeerling.com/blog/2017/testing-your-shell-scripts-bats)