---
title:                "Bash: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenowanie ciągów znaków jest nieodłączną częścią programowania w Bashu. Jest to szczególnie przydatne w przypadku, gdy chcemy połączyć różne fragmenty tekstu w jeden, dłuższy ciąg. Dzięki temu możemy tworzyć bardziej złożone i dynamiczne skrypty, które wykonują konkretne zadania.

## Jak to zrobić

Aby skorzystać z funkcji łączenia ciągów w Bashu, należy użyć operatora plus (`+`). Poniżej znajduje się przykład kodu, który łączy dwa ciągi znaków i wyświetla je jako jeden ciąg:

```Bash
firstName="Jan"
lastName="Kowalski"

echo $firstName + $lastName
```

W wyniku otrzymamy `JanKowalski`. Pamiętaj jednak, że przed i po operatorze plus nie należy umieszczać spacji, ponieważ zostaną one również włączone do wynikowego ciągu.

Możemy także łączyć dłuższe ciągi lub zmienne z innymi wartościami, np. liczbami. Poniżej przedstawiono bardziej złożony przykład:

```Bash
message="Witaj "
name="Maria"
age=30

echo $message $name", masz "$age" lat."
```

W wyniku otrzymujemy komunikat `Witaj Maria, masz 30 lat.`. Warto zauważyć, że tutaj użyto spacji w celu oddzielenia poszczególnych części tekstu.

## Głębsze zagadnienia

Podczas łączenia ciągów znaków w Bashu, warto pamiętać o kilku istotnych zagadnieniach. Po pierwsze, kolejność umieszczania ciągów i wartości jest ważna. Stringi będą łączone w kolejności, w jakiej zostały umieszczone w poleceniu. Po drugie, warto zwrócić uwagę na rodzaj cudzysłowów użytych do przechowywania ciągów. Cudzysłowy podwójne (`"`) pozwalają na użycie zmiennych wewnątrz ciągów, podczas gdy cudzysłowy pojedyncze (`'`) wyświetlają tylko dokładnie to, co zostało wprowadzone. Na koniec, warto zauważyć, że funkcja łączenia ciągów może być użyta również z innymi poleceniami, takimi jak `printf`.

## Zobacz także

- [BashGuide - String Concatenation](https://mywiki.wooledge.org/BashGuide/Practices#String_Concatenation)
- [Bash Tutorial - String Concatenation](https://www.shell-tips.com/bash/string-concatenation-in-bash/)
- [Bash Hackers Wiki - String Manipulation](https://wiki.bash-hackers.org/syntax/pe#substring_removal)