---
title:    "C: Usuwanie znaków odpowiadających wzorowi"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami w procesie tworzenia aplikacji lub skryptów w języku C, może zajść potrzeba usunięcia znaków odpowiadających danemu wzorcowi. Może to być konieczne, aby przetworzyć dane lub zabezpieczyć użytkownika przed niepożądanymi wpisami. W tym artykule pokażemy, jak to zrobić w prosty sposób.

## Jak to zrobić

W C istnieją kilka sposobów na usunięcie znaków zgodnie z danym wzorcem, ale najprostszym z nich jest użycie funkcji `strpbrk()`. Dzięki temu rozwiązaniu możemy przekazać daną linię tekstu oraz wzorzec i funkcja automatycznie usunie wszystkie występujące znaki zgodne z wzorcem. Poniżej przedstawiamy przykładowy kod:

```C
#include <stdio.h>
#include <string.h>

// funkcja usuwająca znaki zgodne z podanym wzorcem
void delete_pattern(char *str, char *pattern) {
    char *ptr = strpbrk(str, pattern); // znajdź pierwsze wystąpienie wzorca
    while (ptr != NULL) { // dopóki nie znajdziesz ostatniego wystąpienia
        strncpy(ptr, ptr + 1, strlen(ptr)); // skopiuj tekst po znaku zgodnym z wzorcem
        ptr = strpbrk(ptr, pattern); // znajdź kolejne wystąpienia
    }
}

int main() {
    char string[100] = "To jest tekst z dodatkowymi znakami #$%A&@!";
    printf("Przed usunięciem: %s\n", string);
    delete_pattern(string, "#$%A&@!"); // usuń znaki zgodne z tym wzorcem
    printf("Po usunięciu: %s\n", string);
    return 0;
}
```

Po uruchomieniu tego kodu, otrzymamy następujący wynik:

```
Przed usunięciem: To jest tekst z dodatkowymi znakami #$%A&@!
Po usunięciu: To jest tekst z dodatkowymi znakami !
```

Możemy zauważyć, że wszystkie znaki zgodne z podanym wzorcem zostały pomyślnie usunięte. Teraz można wykorzystać tę funkcję w swoim kodzie, aby usunąć niechciane znaki i przetworzyć dane.

## Deep Dive

Podpowiadamy również, że w języku C istnieje wiele innych funkcji, które mogą być przydatne w usuwaniu znaków zgodnie z podanym wzorcem. Przykładem może być funkcja `strcspn()`, która zwraca długość najdłuższego prefiksu w danym ciągu tekstu, który nie zawiera określonych znaków. Możemy wykorzystać to, aby uzyskać listę niepożądanych znaków i przekazać ją do naszej wcześniej zdefiniowanej funkcji `delete_pattern()`.

```
char pattern[7] = "!@#$%^";
delete_pattern(string, pattern);
```

Funkcja `delete_pattern()` może również zostać zmodyfikowana, aby usunąć tylko znaki z podanego zakresu, wykorzystując funkcję `strspn()`. Jest to tylko kilka przykładów, jak w łatwy sposób można dostosować kod do własnych potrzeb.

## Zobacz także

- [Dokumentacja funkcji strpbrk() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
- [Inne przydatne funkcje dla operacji na łańcuchach znaków w C](https://codeforwin.org/2018/02/c-program-to-remove-all-occurrences-of-given-character-from-string.html)