---
title:                "Zmienianie litery początkowej w ciągu znaków"
html_title:           "PHP: Zmienianie litery początkowej w ciągu znaków"
simple_title:         "Zmienianie litery początkowej w ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Capitalizowanie (ang. capitalizing) jest procesem zmiany pierwszej litery w słowie na dużą literę. Programiści często to robią, ponieważ jest to standardowa konwencja pisania kodu, co ułatwia czytanie i zrozumienie kodu dla innych programistów.

## Jak to zrobić:

### W PHP:

```php
$string = "witaj świecie!";

echo ucfirst($string); // Wyświetli: "Witaj świecie!"

echo ucwords($string); // Wyświetli: "Witaj Świecie!"
```

## Głębszy Zanurzenie:

### Kontekst Historyczny:

W starszych wersjach PHP zmienną globalną ini_ucfirst można było ustawić na true, aby włączyć automatyczne capitalizowanie pierwszej litery. Jednak w aktualnej wersji PHP nie jest to już dostępna opcja.

### Alternatywy:

Istnieje wiele innych funkcji w PHP umożliwiających manipulowanie tekstem, takich jak strtolower(), strtoupper() czy strrev(). Jednakże, jeśli chodzi o capitalizowanie pierwszej litery, najlepszym wyborem są funkcje ucfirst() lub ucwords().

### Szczegóły Implementacji:

Funkcja ucfirst() zmienia pierwszą literę w stringu na dużą, natomiast ucwords() capitalizuje wszystkie pierwsze litery w każdym słowie w stringu. Funkcje te są wywoływane na zmiennej typu string i zwracają nowy string z zmienionymi literami. Obie funkcje są dostępne od PHP w wersji 4.

## Zobacz też:

- Oficjalna dokumentacja PHP dotycząca funkcji ucfirst(): https://www.php.net/manual/en/function.ucfirst.php
- Oficjalna dokumentacja PHP dotycząca funkcji ucwords(): https://www.php.net/manual/en/function.ucwords.php