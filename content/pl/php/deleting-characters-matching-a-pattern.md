---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "PHP: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Czym i dlaczego?

Usuwanie znaków pasujących do danego wzorca jest jedną z wielu przydatnych funkcji w PHP. Pozwala ona programistom na szybkie i skuteczne filtrowanie danych, na przykład usuwanie niepożądanych znaków ze stringów lub zmianę ich formatowania. Jest to niezwykle przydatna umiejętność w tworzeniu zoptymalizowanych i bezpiecznych skryptów.

## Jak to zrobić?

Załóżmy, że chcemy usunąć wszystkie cyfry z danego stringa. W tym celu możemy użyć funkcji `preg_replace()`, która działa na podstawie wyrażeń regularnych. W poniższym przykładzie użyjemy wyrażenia `/[0-9]/`, które oznacza wszystkie cyfry od 0 do 9. 

```PHP
<?php
$string = "abc123def456ghi";
$new_string = preg_replace("/[0-9]/", "", $string);

echo $new_string; // Output: abcdefghi
?>
```

Funkcja `preg_replace()` przyjmuje trzy parametry: wzorzec do wyszukania, co powinno zostać zastąpione oraz string, w którym ma zostać wykonane wyszukanie. W powyższym przykładzie użyliśmy pustego stringa jako drugiego parametru, ponieważ chcemy, aby znaki zostały po prostu usunięte. 

## Głębszy wgląd

Wyrażenia regularne są jednym z najpotężniejszych narzędzi w PHP. Są one wykorzystywane do szybkiego i precyzyjnego wyszukiwania oraz modyfikacji tekstu. Warto również wspomnieć, że funkcja `preg_replace()` ma kilka alternatywnych wariantów, takich jak `preg_replace_callback()`, które pozwalają na bardziej zaawansowane operacje z wykorzystaniem wyrażeń regularnych.

## Zobacz również

- Oficjalna dokumentacja PHP dotycząca funkcji preg_replace(): https://www.php.net/manual/en/function.preg-replace.php
- Przewodnik dla początkujących na temat wyrażeń regularnych w PHP: https://www.w3schools.com/php/php_regex.asp
- Przykłady zastosowania wyrażeń regularnych w PHP: https://www.tutorialrepublic.com/php-tutorial/php-regular-expressions.php