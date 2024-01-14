---
title:                "Gleam: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Witaj drogi czytelniku, w dzisiejszym wpisie opowiem Ci o jednej z podstawowych operacji, którą można wykonać w języku Gleam - znajdowania długości ciągu znaków. Możesz zapytać dlaczego warto się z tym zapoznać? To proste! Długość ciągu znaków jest często używana w programowaniu, szczególnie gdy pracujemy z wejściem użytkownika lub manipulujemy tekstem.

# Jak to zrobić

Aby znaleźć długość ciągu znaków w języku Gleam możemy użyć wbudowanej funkcji `String.length` oraz podać jej jako argument nasz ciąg znaków. Przykładowo, chcąc znaleźć długość słowa "Programowanie", nasz kod wyglądałby następująco:

```Gleam
String.length("Programowanie")
```

Powyższy kod zwróci nam liczbę `13`, co jest dokładnie długością słowa "Programowanie".

# Głębszy zanurzenie

Sprawdzenie długości ciągu znaków wydaje się być prostym zadaniem, ale w rzeczywistości może być nieco bardziej skomplikowane. W języku Gleam, ciągi znaków są reprezentowane jako listy znaków, więc w istocie używanie funkcji `String.length` sprowadza się do sprawdzenia długości tej listy. Dzięki temu możemy również użyć innych wbudowanych funkcji do manipulacji listami, takich jak `List.map` czy `List.filter`.

# Zobacz też

Oto kilka przydatnych linków, które mogą Ci się przydać przy nauce języka Gleam i programowaniu:

- Dokumentacja języka Gleam: https://gleam.run/documentation/
- Przykłady kodów: https://github.com/gleam-lang/gleam/tree/master/examples
- Kurs programowania w języku Gleam: https://gleam.run/courses/

Dzięki za przeczytanie, mam nadzieję, że po przeczytaniu tego wpisu czujesz się pewniej w znajdowaniu długości ciągu znaków w języku Gleam! Zachęcam również do eksperymentowania z innymi funkcjami i zadań - tylko dzięki praktyce staje się się dobrym programistą.