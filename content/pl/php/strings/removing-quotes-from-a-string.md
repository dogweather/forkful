---
date: 2024-01-26 03:41:49.464968-07:00
description: "Usuwanie cudzys\u0142ow\xF3w z \u0142a\u0144cuch\xF3w znak\xF3w w PHP\
  \ oznacza pozbywanie si\u0119 tych irytuj\u0105cych podw\xF3jnych (`\"`) lub pojedynczych\
  \ (`'`) znak\xF3w cudzys\u0142owu, kt\xF3re\u2026"
lastmod: '2024-03-13T22:44:35.484602-06:00'
model: gpt-4-0125-preview
summary: "Usuwanie cudzys\u0142ow\xF3w z \u0142a\u0144cuch\xF3w znak\xF3w w PHP oznacza\
  \ pozbywanie si\u0119 tych irytuj\u0105cych podw\xF3jnych (`\"`) lub pojedynczych\
  \ (`'`) znak\xF3w cudzys\u0142owu, kt\xF3re mog\u0105 zak\u0142\xF3ca\u0107 logik\u0119\
  \ twojego kodu lub zapytania do bazy danych."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

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
