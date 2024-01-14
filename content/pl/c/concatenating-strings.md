---
title:    "C: Łączenie ciągów znaków"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

##Dlaczego

C jest jednym z najpopularniejszych języków programowania na świecie i ma wiele zastosowań. Jego wszechstronność i możliwość łatwego tworzenia oprogramowania sprawia, że jest on często wybierany przez programistów na całym świecie. Jedną z przydatnych funkcji, które oferuje C, jest konkatenacja (łączenie) ciągów znaków. W tym artykule dowiesz się, czym jest konkatenacja stringów i dlaczego jest to ważne dla Twojej pracy programisty.

##Jak to zrobić

Aby dokonać konkatenacji stringów w języku C, musimy użyć funkcji `strcat ()`. Załóżmy, że mamy dwa stringi: "Cześć" i "świecie". Chcemy połączyć je w jeden ciąg znaków "Cześć świecie". W tym celu użyjemy funkcji `strcat ()` w następujący sposób:

```C
#include <stdio.h>
#include <string.h>
int main()
{
   char str1[] = "Cześć";
   char str2[] = "świecie";
   strcat(str1, str2);
   printf("%s", str1);
   return 0;
}
```

Wynik działania tego programu to "Cześćświecie". Funkcja `strcat ()` łączy dwa stringi i wyświetla je jako jeden ciąg znaków.

##Rozszerzony opis

Konkatenacja stringów jest bardzo przydatną funkcją w języku C. Pozwala nam łączyć dwa lub więcej stringów w jeden ciąg. Ta operacja jest szczególnie przydatna, gdy chcemy wyświetlić na ekranie przyjazną dla użytkownika wiadomość z kilku mniejszych stringów.

Ważne jest jednak, aby pamiętać, że funkcja `strcat ()` nie sprawdzi długości pierwszego stringa i może doprowadzić do przepełnienia bufora, jeśli nie mamy odpowiedniej przestrzeni pomiędzy pierwszym i drugim stringiem. Dlatego zawsze należy upewnić się, że pamięć jest odpowiednio zaalokowana przed użyciem funkcji `strcat ()`.

Inną przydatną funkcją do konkatenacji stringów jest `strncat ()`, która pozwala nam określić maksymalną liczbę znaków, jakie chcemy dodać do pierwszego stringa. Używając tej funkcji możemy uniknąć przepełnienia bufora i zaoszczędzić czas i pamięć.

##Zobacz również

- [Dokumentacja funkcji strcat ()](https://www.tutorialspoint.com/c_standard_library/c_function_strcat)
- [Przykłady użycia funkcji strcat ()](https://www.programiz.com/c-programming/library-function/string.h/strcat)