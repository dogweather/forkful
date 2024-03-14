---
date: 2024-01-20 17:35:15.931676-07:00
description: "\u0141\u0105czenie string\xF3w to po prostu sklejanie dw\xF3ch lub wi\u0119\
  cej tekst\xF3w w jeden. Programi\u015Bci robi\u0105 to, aby budowa\u0107 wiadomo\u015B\
  ci, tworzy\u0107 zapytania SQL, a nawet\u2026"
lastmod: '2024-03-13T22:44:35.488995-06:00'
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie string\xF3w to po prostu sklejanie dw\xF3ch lub wi\u0119\
  cej tekst\xF3w w jeden. Programi\u015Bci robi\u0105 to, aby budowa\u0107 wiadomo\u015B\
  ci, tworzy\u0107 zapytania SQL, a nawet\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Łączenie stringów to po prostu sklejanie dwóch lub więcej tekstów w jeden. Programiści robią to, aby budować wiadomości, tworzyć zapytania SQL, a nawet zarządzać wyświetlanymi adresami URL – w zasadzie wszędzie tam, gdzie dynamika i elastyczność tekstu są potrzebne.

## Jak to zrobić:

Łączenie stringów w PHP jest banalnie proste, użyj po prostu kropki:

```PHP
$powitanie = 'Cześć';
$imie = 'Janek';
$zdanie = $powitanie . ', ' . $imie . '!';
echo $zdanie;
```

Wyjście (output):

```
Cześć, Janek!
```

A jeśli lubisz bardziej nowoczesne rozwiązania, od PHP 5.5 możesz użyć operatora złączenia (`.=`) do dodania stringa do istniejącego:

```PHP
$zdanie = 'Programowanie w PHP ';
$zdanie .= 'jest fajne!';
echo $zdanie;
```

Wyjście (output):

```
Programowanie w PHP jest fajne!
```

## W głębi tematu:

### Historia
W PHP, jak w wielu innych językach, od zawsze istniała potrzeba pracy z tekstami. W prastarych czasach PHP, przed erą HTML i powszechnej komunikacji sieciowej, łączenie stringów miało już swoje miejsce.

### Alternatywy
Możesz też użyć funkcji `sprintf()` albo `printf()` do formatowania i łączenia stringów, co może być szczególnie użyteczne przy większej ilości zmiennych:

```PHP
$nazwa = 'świat';
$format = 'Witaj, %s!';
echo sprintf($format, $nazwa);
```

Wyjście (output):

```
Witaj, świat!
```

### Realizacja
Wewnętrznie, stringi w PHP są mutowalne, ale z każdą operacją łączenia tworzony jest nowy string, co może wpłynąć na używanie pamięci gdy łączy się bardzo dużo i długie stringi.
