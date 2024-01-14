---
title:    "Fish Shell: Znajdowanie długości ciągu znaków"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak policzyć długość tekstu w swoim kodzie? Może chcesz poznać długość nazwy pliku lub ścieżki, lub po prostu sprawdzić, czy łańcuch znaków jest wystarczająco długi? W tym artykule dowiecie się, jak w prosty sposób wykonać tę czynność w środowisku Fish Shell.

## Jak to zrobić?

Zacznijmy od zdefiniowania zmiennej przechowującej nasz tekst. W tym przypadku będzie to "Hello World!". Następnie użyjmy wbudowanej funkcji `string length` (długość łańcucha) i przekażmy jako parametr naszą zmienną tekstową:

```Fish Shell
set text "Hello World!"
string length $text
```

To powinno zwrócić wynik 12, ponieważ nasz tekst składa się z 12 znaków. Możemy również odwoływać się do konkretnych znaków w tekście za pomocą `string sub` (dóbr znaku) i `count` (licznika). Na przykład, jeśli chcemy poznać długość tylko pierwszych 5 znaków, możemy to zrobić w ten sposób:

```Fish Shell
set text "Hello World!"
string length (string sub -c 0 5 $text)
```

To zwróci wynik 5, ponieważ liczy tylko pierwsze 5 znaków.

## Deep Dive

W Fish Shell, funkcja `string length` wykorzystuje wewnętrzne narzędzia systemowe do obliczania długości tekstu. Jest ona bardzo szybka i wydajna, więc nie musisz się martwić o wpływ na wydajność swojego kodu.

Ponadto, funkcja `string length` jest bardzo przydatna w wielu różnych kontekstach. Możesz jej użyć w skryptach, wykorzystać w funkcjach, czy nawet w interaktywnej sesji Fish Shell.

## Zobacz także
- Dokumentacja Fish Shell: https://fishshell.com/docs/current/cmds/string.html#string-length
- Przydatne funkcje Fish Shell: https://fishshell.com/docs/current/index.html#builtin-functions
- Blog Fish Shell: https://fishshell.com/blog/