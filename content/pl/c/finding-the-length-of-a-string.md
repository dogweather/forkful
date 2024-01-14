---
title:                "C: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Długość ciągu znaków jest jedną z podstawowych operacji w C, która jest niezbędna do zrozumienia dla każdego programisty. Pozwala nam ona określić ilość znaków w danym ciągu, co jest niezbędne do wielu zadań, takich jak kopiowanie, łączenie lub weryfikacja tekstu. Dlatego też, poznając sposoby na znajdowanie długości ciągu znaków, staje się nam dostępnych wiele nowych możliwości programistycznych.

## Jak to zrobić

Istnieje kilka sposobów na znalezienie długości ciągu znaków w języku C. Jednym z najprostszych sposobów jest użycie funkcji `strlen()` z biblioteki standardowej `string.h`. Funkcja ta przyjmuje jako argument wskaźnik na ciąg znaków i zwraca jego długość jako liczbę całkowitą. Poniżej znajduje się przykładowy kod wykorzystujący tę funkcję:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char exampleString[] = "Przykładowy tekst";
    printf("Długość ciągu znaków wynosi: %d", strlen(exampleString));
    return 0;
}
```

Output:
`Długość ciągu znaków wynosi: 18`

Innym sposobem na znalezienie długości ciągu jest użycie pętli `while` w połączeniu z funkcją `sizeof()`. Poniższy kod pokazuje jak można to zrobić:

```C
#include <stdio.h>

int main()
{
    char exampleString[] = "Przykładowy tekst";
    int i = 0;
    while(exampleString[i] != '\0')
    {
        i++;
    }
    printf("Długość ciągu znaków wynosi: %d", i);
    return 0;
}
```

Output:
`Długość ciągu znaków wynosi: 18`

## Wnikliwa analiza

Warto zauważyć, że funkcja `strlen()` liczy znaki od pierwszego aż do znaku końca ciągu, który jest oznaczony przez specjalny znak `\0`. Dzięki temu, możemy umieścić wiele znaków w jednej tablicy, a funkcja ta i tak poprawnie obliczy długość ciągu. Ponadto, pętla `while` w połączeniu z funkcją `sizeof()` sprawdzają każdy kolejny bajt aż do napotkania znaku `\0`, co w praktyce jest równoznaczne ze zliczeniem wszystkich znaków.

## Zobacz również

- Dokumentacja funkcji `strlen()` na stronie [cplusplus.com](https://www.cplusplus.com/reference/cstring/strlen/)
- Wideo tutorial o znajdowaniu długości ciągu znaków w C na kanale [Programiz](https://www.programiz.com/c-programming/c-strings-length) 
- Przykładowe zadanie dotyczące znajdowania długości ciągu znaków na [HackerRank](https://www.hackerrank.com/challenges/strlen-in-c/problem)