---
title:                "Bash: Łączenie ciągów znaków"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy, kto programuje w języku Bash, pewnie już spotkał się z problemem łączenia kilku ciągów tekstu w jeden. Rozwiązaniem tego problemu jest funkcja concatenacji, czyli połączenia jednego stringa z drugim. W tym artykule dowiecie się, dlaczego jest to przydatne oraz jak tego dokonać.

## Jak to zrobić

Aby połączyć dwa stringi w jednym, niezbędna jest funkcja concat. W poniższym przykładzie użyjemy trzech zmiennych, które zawierają tekst, a następnie połączymy je w jeden string.

```Bash
variable1="Witaj"
variable2="świecie!"
variable3="Jestem programistą Bash"
concat="$variable1 $variable2. $variable3"
echo "$concat"
```

Wynik tego kodu to "Witaj świecie!. Jestem programistą Bash". Można również dodawać spację lub inne znaki specjalne, aby oddzielić poszczególne zmienne.

## Deep Dive

Funkcja concatenacji może być wykorzystywana na wiele sposobów. Jest szczególnie przydatna w przypadku, gdy potrzebujemy połączyć wiele zmiennych w jednym dłuższym ciągu tekstowym. Można również wykorzystać tę technikę do tworzenia dynamicznych komunikatów lub w celu tworzenia bardziej czytelnych kodów.

W przypadku gdy chcemy połączyć więcej niż dwa stringi, możemy wykorzystać polecenie `concat+="tekst"`, aby dodać kolejny string do już istniejącego. Jest to również przydatne w przypadku, gdy nie znamy dokładnej ilości zmiennych do połączenia.

## Zobacz też

1. [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/)
2. [Artykuł o zmiennej concat na Medium](https://medium.com/linuxforeveryone/concatenation-and-variables-in-bash-fc20ca32270)