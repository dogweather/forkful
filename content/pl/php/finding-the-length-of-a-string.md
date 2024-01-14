---
title:    "PHP: Znajdowanie długości ciągu znaków"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeden z podstawowych zadań, z którymi spotykamy się w programowaniu, to operacje na ciągach znaków. Jedną z nich jest znajdowanie długości ciągu - jest to niezbędny krok, aby wiedzieć ile znaków zawiera dany napis. W tym artykule pokażemy, jak w prosty sposób uzyskać długość ciągu w języku PHP.

## Jak to zrobić

Aby uzyskać długość ciągu w PHP, możemy skorzystać z funkcji `strlen()`. Przyjmuje ona jako argument ciąg znaków, a zwraca jego długość jako wartość całkowitą. Przykładowe użycie tej funkcji wygląda w następujący sposób:

```PHP
<?php
$text = "Witaj świecie!";
echo strlen($text); // wypisze: 15
``` 

W powyższym przykładzie najpierw tworzymy zmienną `$text`, do której przypisujemy napis "Witaj świecie!". Następnie wykorzystujemy funkcję `strlen()` do obliczenia długości tego napisu i wypisujemy wynik za pomocą funkcji `echo`. Proste, prawda?

## Głębszy zanurzenie

W przypadku, gdy chcemy liczyć długość ciągu zawierającego znaki niedwubajtowe, tj. nie będące jednoznacznymi znakami Unicode, musimy sięgnąć po funkcję `mb_strlen()`. Km jest ona dostępna tylko wtedy, gdy rozszerzenie `mbstring` jest załadowane w PHP. Przykładowe wykorzystanie tej funkcji wygląda następująco:

```PHP
<?php
$text = "ąśćęźżłółń";
echo mb_strlen($text, 'UTF-8'); // wypisze: 10
```

Funkcja `mb_strlen()` przyjmuje dwa argumenty - pierwszy to ciąg znaków, a drugi (opcjonalny) to kodowanie, w którym znajduje się ten ciąg. Dzięki temu możemy bez problemu obliczać długość nawet najbardziej egzotycznych napisów.

## Zobacz także

- [Dokumentacja funkcji `strlen()` w PHP](https://www.php.net/manual/en/function.strlen.php)
- [Dokumentacja funkcji `mb_strlen()` w PHP](https://www.php.net/manual/en/function.mb-strlen.php)