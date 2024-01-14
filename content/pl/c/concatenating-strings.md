---
title:                "C: Łączenie ciągów znaków"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Podczas programowania w języku C często spotykamy się z potrzebą połączenia dwóch lub więcej łańcuchów znaków. Ten proces, nazywany konkatenacją, może być bardzo przydatny w różnych sytuacjach, na przykład w tworzeniu interfejsów użytkownika lub wypisywaniu komunikatów.

## Jak to zrobić

Połączenie dwóch lub więcej łańcuchów znaków w języku C można zrealizować za pomocą funkcji strcat(). W poniższym przykładzie pokazane jest, jak połączyć dwa łańcuchy znaków i wyświetlić wynik:

```C
#include <stdio.h>
#include <string.h>

int main() {
	char str1[] = "Hello";
	char str2[] = "world!";
	
	strcat(str1, str2);
	printf("%s\n", str1);
	
	return 0;
}
```

Ten kod najpierw importuje bibliotekę string.h, która zawiera funkcję strcat(). Następnie definiujemy dwa łańcuchy znaków str1 i str2. Funkcja strcat() łączy te dwa łańcuchy, dodając zawartość str2 na koniec str1. W rezultacie otrzymujemy łańcuch "Helloworld!", który jest następnie wyświetlany na ekranie.

## Głębszy przegląd

Funkcja strcat() działa na zasadzie wykonywania konkatenacji od drugiego łańcucha do pierwszego. Oznacza to, że zawartość drugiego łańcucha jest dodawana na koniec pierwszego łańcucha, zastępując jego znak końca '\0'. Jest to ważne, ponieważ jeśli pierwszy łańcuch nie ma wystarczającej ilości miejsca, funkcja ta może spowodować przepisywanie pamięci i spowodować niestabilność programu.

Aby uniknąć niepożądanych konsekwencji, zaleca się również użycie funkcji strncpy() lub strncat(), które pozwalają określić maksymalną długość łańcucha wynikowego i unikają przepisywania pamięci.

## Zobacz także

- [Dokumentacja funkcji strcat() w języku C](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [Przewodnik po konkatenacji w języku C](https://www.tutorialspoint.com/cprogramming/c_string_concate.htm)