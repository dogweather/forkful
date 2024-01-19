---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Kasowanie znaków według wzorca

## Co i dlaczego?
Usuwanie znaków według wzorca to sposób na edycję tekstu poprzez kasowanie konkretnych znaków. Programiści stosują to do odrzucania niepotrzebnych lub niechcianych danych, porządkowania napisów i czyszczenia danych wejściowych.

## Jak to zrobić?
Oto przykład kodu ilustrujący, jak w PHP można usunąć znaki według wzorca:

```PHP
<?php
$string = "Hello, świat!";
$pattern = "/[aeiou]/i";
$replacement = "";
echo preg_replace($pattern, $replacement, $string);
?>
```

Gdy uruchomisz ten kod, wynik będzie następujący:

```PHP
Hll, śwt!
```

Jak widzisz, usuwamy samogłoski z naszego ciągu `$string`, korzystając ze standardowego wzorca `preg_replace()` do usuwania znaków.

## Na głębi
1. **Kontekst historyczny**: Usuwanie znaków według wzorca istnieje od początków programowania i jest powszechnie używane we wszystkich językach programowania.
2. **Alternatywy**: Istnieje wiele sposobów usuwania znaków według wzorca. W PHP, najpopularniejszym jest funkcja `preg_replace()`. Inne metody obejmują użycie `str_replace()`, ale to nie jest tak potężne jak preg_replace(), który obsługuje wyrażenia regularne.
3. **Szczegóły implementacji**: Funkcja `preg_replace()` w PHP to potężne narzędzie, które pozwala na korzystanie z wyrażeń regularnych do definiowania wzorców. To daje programistom pełną kontrolę nad tym, co i jak chcą usuwać.

## Zobacz też
1. `preg_replace()`: [https://www.php.net/manual/pl/function.preg-replace.php](https://www.php.net/manual/pl/function.preg-replace.php)
2. `str_replace()`: [https://www.php.net/manual/pl/function.str-replace.php](https://www.php.net/manual/pl/function.str-replace.php)
3. Wyrażenia regularne w PHP: [https://www.php.net/manual/pl/book.pcre.php](https://www.php.net/manual/pl/book.pcre.php)