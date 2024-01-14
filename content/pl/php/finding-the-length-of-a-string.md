---
title:                "PHP: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy w trakcie programowania może pojawić się potrzeba zbadania długości łańcucha znaków. Bez względu na to czy jest to potrzebne do walidacji danych czy też do logiki biznesowej, znajomość sposobu na ustalenie długości łańcucha znaków jest niezbędna w codziennym życiu programisty.

## Jak to zrobić

```PHP
$string = "Cześć, to jest przykładowy łańcuch znaków";

echo "Długość łańcucha: " . strlen($string);
```
**Output:**
Długość łańcucha: 36

Funkcja `strlen()` jest używana do zwrócenia długości łańcucha znaków. Może być używana zarówno do zwykłych łańcuchów znaków, jak i zmiennych przechowujących dane tekstowe. Istnieje też możliwość użycia tej funkcji do wyznaczenia długości tablicy.

Jeśli interesuje nas długość łańcucha znaków w bajtach, możemy użyć funkcji `mb_strlen()` z modułu rozszerzeń Multibyte String.

```PHP
$string = "Łódź";

echo "Długość łańcucha w bajtach: " . mb_strlen($string);
```
**Output:**
Długość łańcucha w bajtach: 6

## Deep Dive

Funkcja `strlen()` jest bardzo prosta w użyciu i zwraca dokładną długość łańcucha znaków. Jest to możliwe, ponieważ łańcuchy znaków w PHP są przechowywane wewnątrz specjalnych struktur danych znanymi jako ZVAL (zend value). Te struktury zawierają nie tylko same znaki, ale także dodatkowe informacje, takie jak długość łańcucha. Dzięki temu PHP może szybko i skutecznie zwrócić długość łańcucha, bez konieczności przeliczania go na bieżąco.

Jednak istnieje pewne zachowanie, o którym warto wiedzieć. Funkcja `strlen()` zwróci długość łańcucha w bajtach, a nie w znakach. Oznacza to, że jeśli używasz kodowania znaków, które wykorzystuje znaki wielobajtowe (np. UTF-8), długość może być nieco inna, niż się tego spodziewasz.

## Zobacz także
- [Dokumentacja funkcji `strlen()` na php.net](https://www.php.net/manual/en/function.strlen.php)
- [Dokumentacja funkcji `mb_strlen()` na php.net](https://www.php.net/manual/en/function.mb-strlen.php)
- [Inne funkcje związane z manipulacją łańcuchami znaków w PHP na phppopolsku.pl](https://phppopolsku.pl/funkcje/lanuchy-znakow)