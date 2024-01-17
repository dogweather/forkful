---
title:                "Interpolacja ciągu znaków"
html_title:           "PHP: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to Jest i Dlaczego Robimy To?

Interpolacja ciągu znaków w PHP jest procesem wstawiania wartości zmiennych lub wyrażeń wewnątrz ciągu znaków. Programiści wykorzystują to do dynamicznego tworzenia tekstów lub wyrażeń, które zawierają zmienne lub wyniki obliczeń.

## Jak to Zrobić:

```PHP
$name = "Karol";
echo "Cześć $name, jak się masz?";
// wynik: Cześć Karol, jak się masz?
```

W powyższym przykładzie zmienna ```$name``` jest wstawiana wewnątrz ciągu znaków za pomocą symbolu ```$```. Można również wykorzystać inny sposób zamykający ciąg znaków, w postaci ```${} ```, aby jawnie oddzielić zmienną od reszty tekstu.

```PHP
$score = 90;
echo "Twój wynik to ${score}/100";
// wynik: Twój wynik to 90/100
```

W przypadku wyrażeń, wstawianie odbywa się poprzez umieszczenie ich w nawiasach klamrowych wewnątrz ciągu znaków.

```PHP
$x = 10;
$y = 5;
echo "Suma ${x} + ${y} = ${x + $y}";
// wynik: Suma 10 + 5 = 15
```

## Wnikliwa Analiza:

Interpolacja ciągu znaków została wprowadzona w języku PHP w wersji 4.0, jako alternatywa dla funkcji ```sprintf()```. W porównaniu do tej funkcji, interpolacja jest szybsza i łatwiejsza w użyciu, zwłaszcza w przypadku wielu zmiennych.

Alternatywą dla interpolacji jest konkatenacja, czyli łączenie fragmentów tekstu i zmiennych za pomocą operatora kropki (```.```).

```PHP
$name = "Karol";
echo "Cześć " . $name . ", jak się masz?";
// wynik: Cześć Karol, jak się masz?
```

W przypadku konkatenacji, konieczne jest ręczne dodawanie separatorów (takich jak spacje) w ciągu znaków, aby uzyskać czytelny tekst.

W implementacji, interpolacja ciągu znaków jest rozwiązywana za pomocą funkcji ```str_replace()```, co może wydawać się mniej wydajne dla większych ilości danych.

## Zobacz też:

- Dokumentacja PHP o [interpolacji ciągu znaków](https://www.php.net/manual/pl/language.types.string.php#language.types.string.syntax.double)
- Przydatny poradnik o [wstawianiu zmiennych w ciągu znaków w PHP](https://www.geeksforgeeks.org/how-to-insert-php-variables-inside-a-javascript-code/)