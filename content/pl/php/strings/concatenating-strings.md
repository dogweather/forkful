---
title:                "Łączenie łańcuchów znaków"
aliases:
- /pl/php/concatenating-strings.md
date:                  2024-01-20T17:35:15.931676-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
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
