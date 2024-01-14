---
title:                "C: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wynajmowanie podciągów jest ważną częścią programowania w języku C. Pozwala ono na wyodrębnienie pewnej części tekstu z danego ciągu znaków, co jest niezbędne w wielu projektach. W tym poście dowiesz się, dlaczego warto używać tej funkcji i jak to zrobić.

## Jak to zrobić

Aby wydobyć podciąg w języku C, należy użyć funkcji `substr`, która jest dostępna w większości kompilatorów. Przede wszystkim, należy określić wejściowy ciąg i indeks początkowy oraz końcowy podciągu. Następnie, można wyświetlić go na ekranie przy użyciu funkcji `printf`. Przykładowy kod znajduje się poniżej:

```C
#include <stdio.h>

int main() {
    char input[] = "Ten post jest świetny!";
    int start = 4;
    int end = 8;
    char output[end - start + 1];

    // Wybranie podciągu
    for (int i = start; i < end; i++) {
        output[i - start] = input[i];
    }
    // Dodanie terminatora znaku
    output[end - start] = '\0';

    // Wyświetlenie wyniku
    printf("%s", output);

    return 0;
}
```

Powyższy kod wyświetli podciąg "post" na ekranie. Jest to wynik przetwarzania wejściowego tekstu "Ten post jest świetny!" przez kod, który wybiera tylko znaki z indeksami od 4 do 8.

## Wnikliwa analiza

Aby lepiej zrozumieć funkcję `substr` i z jej właściwościami, warto wiedzieć, że w języku C indeksowanie zaczyna się od 0. Oznacza to, że pierwszy znak w ciągu ma indeks 0, drugi - 1 itd. Aby określić podciąg, musimy ustalić indeks początkowy i końcowy, ale zwykle musimy również wiedzieć, czy ostatni znak jest włączony do podciągu czy nie. W przypadku naszego przykładu, musieliśmy użyć `end - start + 1` w celu zapewnienia, że ostatni znak jest wliczony.

Inną przydatną funkcją jest `strlen`, która zwraca długość ciągu znaków oraz `strcpy`, która umożliwia kopiowanie tekstu z jednej zmiennej do drugiej. Te dwie funkcje mogą być przydatne przy pracy z podciągami.

Inną istotną rzeczą do zapamiętania jest to, że podciągi w języku C muszą być zawsze kończone terminatorami znaków `\0`. Jest to konieczne ze względu na to, jak język C obsługuje ciągi znaków. Bez dodania terminatora, funkcje operujące na ciągach mogą sprawiać problemy lub nie działać poprawnie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat pracy z ciągami znaków w języku C, możesz zerknąć na następujące linki:

- [Dokumentacja języka C](https://www.tutorialspoint.com/cprogramming/index.htm)
- [Funkcje dla obsługi ciągów znaków w języku C](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)
- [Tutorial video na temat wybierania podciągów w języku C](https://www.youtube.com/watch?v=cG30ON6iQyM)