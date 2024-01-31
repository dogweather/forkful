---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:41:49.464968-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie cudzysłowów z łańcuchów znaków w PHP oznacza pozbywanie się tych irytujących podwójnych (`"`) lub pojedynczych (`'`) znaków cudzysłowu, które mogą zakłócać logikę twojego kodu lub zapytania do bazy danych. Programiści robią to, aby oczyścić lub zabezpieczyć dane wejściowe, zapewniając bezpieczne używanie lub przechowywanie łańcuchów znaków.

## Jak to zrobić:
Oto prosty przykład z wykorzystaniem wbudowanych funkcji PHP:

```php
$quotedString = "'Cześć,' powiedziała, \"To jest piękny dzień!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Wyświetla: Cześć, powiedziała, To jest piękny dzień!
```

Proste, prawda? Funkcja `str_replace()` przyjmuje tablicę znaków do usunięcia z łańcucha, w tym zarówno pojedyncze, jak i podwójne cudzysłowy.

## Głębsze spojrzenie
W początkowych czasach PHP, programiści musieli być szczególnie ostrożni z cudzysłowami w łańcuchach znaków, zwłaszcza przy wstawianiu danych do bazy danych. Niepoprawnie obsłużone cudzysłowy mogły prowadzić do ataków iniekcji SQL. Wtedy pojawiły się magiczne cudzysłowy, funkcja, która automatycznie ekranowała dane wejściowe. Została uznana za przestarzałą i ostatecznie usunięta, ponieważ zachęcała do złych praktyk programistycznych i problemów z bezpieczeństwem.

Teraz używamy funkcji takich jak `str_replace()` lub regex z `preg_replace()` dla bardziej zaawansowanych wzorców. Oto przykład z wykorzystaniem regex:

```php
$quotedString = "'Cześć,' powiedziała, \"To jest piękny dzień!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Dla danych JSON możesz użyć `json_encode()` z opcjami takimi jak `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`, aby uniknąć dodatkowych ukośników w twoich cudzysłowach.

Przy implementacji, weź pod uwagę przypadki skrajne. Co jeśli twój łańcuch znaków ma zawierać pewne cudzysłowy, jak dialogi w opowiadaniu czy cale w pomiarach? Kontekst ma znaczenie, więc dostosuj usuwanie cudzysłowów do zamierzonego użytku danych.

## Zobacz także
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: Zapobieganie iniekcjom SQL](https://owasp.org/www-community/attacks/SQL_Injection)
