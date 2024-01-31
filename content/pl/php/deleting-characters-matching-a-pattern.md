---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:42:42.774548-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków odpowiadających określonemu wzorcowi to po prostu filtrowanie tekstu. Programiści robią to, by oczyścić dane, usunąć niepotrzebne elementy, lub przygotować tekst do dalszej obróbki.

## Jak to zrobić:
Do usunięcia znaków używa się często wyrażeń regularnych z funkcją `preg_replace`. Oto przykład:

```php
$text = "Witaj, Świecie! 1234";
$pattern = '/[0-9]+/';
$replacement = '';

$cleanedText = preg_replace($pattern, $replacement, $text);

echo $cleanedText; // Wynik: "Witaj, Świecie! "
```

Wyrażenie regularne `[0-9]+` oznacza, że znajdziemy wszystkie miejsca z jednym lub więcej cyframi i zamienimy je na pusty string, czyli usuniemy.

## Deep Dive
Usuwanie znaków to nie tylko `preg_replace`. Historia pokazuje, że w PHP używano też `str_replace` do prostszych zadań, bez wyrażeń regularnych. Alternatywą jest też `filter_var` z flagą `FILTER_SANITIZE_STRING`, ale działa mniej elastycznie. W implementacji `preg_replace` ważne jest, by zrozumieć składnię wyrażeń regularnych – wzorce to klucz do mocy tej funkcji.

## Zobacz także
- [PHP Manual: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP Manual: Regex syntax](https://www.php.net/manual/en/pcre.pattern.php)
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: filter_var](https://www.php.net/manual/en/function.filter-var.php)
