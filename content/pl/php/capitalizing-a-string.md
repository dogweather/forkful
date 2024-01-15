---
title:                "Zapisywanie wielkich liter w ciągu znaków"
html_title:           "PHP: Zapisywanie wielkich liter w ciągu znaków"
simple_title:         "Zapisywanie wielkich liter w ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czemu warto wykorzystywać funkcję 'strtoupper' do zamieniania liter na duże litery? Przez wykorzystanie tego prostego narzędzia można szybko i sprawnie zmienić format tekstu do swoich potrzeb. Ponadto, jest to bardzo przydatne przy pracy z tekstami w programowaniu.

## Jak to zrobić

Kodowanie w PHP jest bardzo proste i intuicyjne, dlatego też wykorzystanie funkcji 'strtoupper' nie sprawia większych trudności. Wystarczy umieścić kod pomiędzy znacznikami "```PHP" i "```" oraz użyć funkcji w następujący sposób:

```PHP
$name = "programowanie w PHP";
echo strtoupper($name);
```

W rezultacie otrzymamy: PROGRAMOWANIE W PHP.

## Głębszy zanurzenie

Funkcja 'strtoupper' w PHP służy do zamiany wszystkich liter w podanym ciągu znaków na duże litery. Dzięki temu możemy szybko i bez większego wysiłku zmienić format tekstu, co jest szczególnie przydatne przy tworzeniu raportów, dokumentów czy formularzy.

Funkcja ta jest również bardzo użyteczna przy porównywaniu tekstu, ponieważ zamiana na jeden, ustalony format ułatwia znalezienie identycznych lub podobnych ciągów znaków.

Innym ciekawym zastosowaniem może być wykorzystanie funkcji 'strtoupper' do zabezpieczenia formularzy na stronie internetowej. W przypadku, gdy wymagane jest wpisanie kodu captcha, zmiana liter na duże znacznie zmniejsza szansę na błędy przy wpisywaniu kodu.

## Zobacz także

- Dokumentacja funkcji 'strtoupper' w PHP: https://www.php.net/manual/en/function.strtoupper.php
- Przykłady użycia funkcji 'strtoupper': https://www.w3schools.com/php/func_string_strtoupper.asp
- Poradnik dotyczący tworzenia formularzy z wykorzystaniem funkcji 'strtoupper': https://www.phpzag.com/uppercasing-captcha-code-by-strtoupper-function-in-php/