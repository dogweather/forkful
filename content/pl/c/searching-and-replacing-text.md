---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "C: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Cześć programiści!

Chcesz wiedzieć, co to jest wyszukiwanie i zamiana tekstu oraz dlaczego jest istotne dla programistów? Świetnie, bo właśnie o tym będzie ten artykuł!

## Co i dlaczego?

Wyszukiwanie i zamiana tekstu to częsta czynność wśród programistów. Polega ona na znalezieniu określonego tekstu i zastąpieniu go innym. Jest to przydatne w przypadku, gdy masz do czynienia z dużym plikiem lub kodem, w którym trzeba wprowadzić zmiany w wielu miejscach.

Dlaczego programiści to robią? Ponieważ jest to szybki i wygodny sposób na wprowadzanie zmian. Zamiast przeszukiwać cały kod ręcznie, można użyć wyszukiwarki i zastąpić tekst jednym kliknięciem.

## Jak to zrobić?

```C
#include <stdio.h>

int main()
{
    char string[] = "Cześć programiści!";
    char search[] = "programiści";
    char replace[] = "koderzy";
    
    //wyszukiwanie i zmiana tekstu
    int i, j, flag;
    for (i = 0; string[i] != '\0'; ++i)
    {
        flag = 1;
        for (j = 0; search[j] != '\0'; ++j)
        {
            if (string[i + j] != search[j])
            {
                flag = 0;
                break;
            }
        }
        if (flag)
        {
            for (j = 0; replace[j] != '\0'; ++j)
            {
                string[i + j] = replace[j];
            }
            break;
        }
    }
    
    //wyświetlenie wyniku
    printf("%s", string);
    
    return 0;
}
```

Output: Cześć koderzy!

Zobacz jak proste to jest? Teraz możesz wykorzystać ten kod do dokonywania zmian w swoim kodzie.

## Głębsze zanurzenie

Wyszukiwanie i zamiana tekstu jest popularną praktyką, ale nie jest jedyną metodą na wprowadzanie zmian w kodzie. Istnieją również narzędzia, takie jak edytory tekstu, które posiadają funkcję wyszukiwania i zamiany. Jednak dla programistów, którzy muszą często dokonywać zmian w kodzie, pisanie własnej funkcji jest szybszym i bardziej wygodnym rozwiązaniem.

Co więcej, istnieje możliwość wykorzystania wyrażeń regularnych do wyszukiwania i zamiany tekstu. Jest to bardziej zaawansowana metoda, ale jeśli nauczyć się jej poprawnie używać, może przyspieszyć proces.

Jeśli chcesz poznać więcej o wyszukiwaniu i zamianie tekstu w języku C, zapraszam do lektury dokumentacji C: https://en.cppreference.com/w/c/string/byte

## Zobacz również

Jeśli chcesz poznać więcej sposobów na wykorzystanie funkcji wyszukiwania i zamiany tekstu w języku C, możesz przeczytać artykuł na temat wyrażeń regularnych w C: https://www.geeksforgeeks.org/regular-expressions-in-c/

Dziękuję za przeczytanie tego artykułu. Mam nadzieję, że teraz czujesz się pewniej w wyszukiwaniu i zamianie tekstu w języku C. Powodzenia z programowaniem! 😊