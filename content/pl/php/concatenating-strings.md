---
title:    "PHP: Łączenie ciągów tekstowych"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego
Concatenacja, czyli łączenie kilku ciągów znaków w jeden, jest niezbędnym elementem w programowaniu PHP. Używamy jej do tworzenia dynamicznych treści, takich jak wyświetlanie komunikatów lub tworzenie adresów URL. Dzięki niej nasz kod staje się bardziej wydajny i czytelny.

## Jak to zrobić
```PHP
// Przykład 1
$greeting = "Cześć";
$name = "Jan";
$message = $greeting . ", " . $name . "! Witaj w naszej aplikacji!";
echo $message;
```
```
// Wynik przykładu 1:
Cześć, Jan! Witaj w naszej aplikacji!
```

```PHP
// Przykład 2
$first_name = "Anna";
$last_name = "Nowak";
$email = $first_name . "." . $last_name . "@example.com";
echo $email
```
```
// Wynik przykładu 2:
anna.nowak@example.com
```

## Głębsze spojrzenie
Concatenacja może być wykorzystana również do budowania złożonych zapytań SQL, gdzie musimy połączyć różne elementy, takie jak nazwa tabeli i warunki. Może również być używana do wyświetlania danych z bazy danych, gdzie musimy połączyć kilka wartości w jednym wyświetlanym ciągu. W przypadku bardziej skomplikowanych operacji, warto wykorzystać funkcję `implode()` lub `sprintf()`, aby uniknąć błędów w kodzie i ułatwić późniejsze modyfikacje.

## Zobacz także
- [PHP Manual - Concatenation Operator](https://www.php.net/manual/en/language.operators.string.php)
- [Concatenation vs Interpolation in PHP](https://www.codecademy.com/articles/concatenation-vs-interpolation)
- [PHP Concatenation Tutorial](https://www.w3schools.com/php/php_operators.asp)