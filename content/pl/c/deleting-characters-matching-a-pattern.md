---
title:    "C: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w języku C może zajść potrzeba usunięcia znaków zgodnych z określonym wzorem. Powodów może być wiele, na przykład chcemy oczyścić tekst z niepotrzebnych znaków lub szybko przetworzyć duże ilości danych. W tym artykule omówimy, jak dokonać tego w prosty sposób.

## Jak to zrobić

Aby usunąć znaki pasujące do wzoru w języku C, musimy wykorzystać funkcję `strcspn`. Przyjmuje ona jako parametry dwa łańcuchy znaków - pierwszy jest źródłem, z którego będą usuwane znaki, a drugi to wzorzec, według którego ma nastąpić usunięcie. Przykładowy kod może wyglądać następująco:
```C
#include <stdio.h>
#include <string.h>

int main() {
    char source[] = "Hello World!";
    char pattern[] = "ld";
    char * result = strcspn(source, pattern);
    // result = 8, bo od indeksu 8 w źródle znajduje się pierwszy znak niepasujący do wzorca
    source[result] = '\0';
    printf("%s\n", source); // wypisze "Hello Wor"
    return 0;
}
```

## Głębszy przegląd

Funkcja `strcspn` zwraca indeks pierwszego znaku niepasującego do wzorca w łańcuchu lub długość łańcucha, jeśli wszystkie znaki pasują. Możemy wykorzystać to w pętli do usunięcia wszystkich pasujących znaków. Przykładowy kod może wyglądać w ten sposób:
```C
#include <stdio.h>
#include <string.h>

int main() {
    char source[] = "Hello World!";
    char pattern[] = "ld";
    char * result = source;
    while (strcspn(result, pattern) != strlen(source)) {
        // znajdź kolejny znak niepasujący do wzorca
        size_t index = strcspn(result, pattern);
        // skopiuj pozostałe znaki do poprzedniego miejsca w łańcuchu
        memmove(result + index, result + index + 1, strlen(result) - index);
    }
    printf("%s\n", source); // wypisze "Heo Wor!"
    return 0;
}
```

Warto również pamiętać, że funkcja `strcspn` jest dostępna tylko w bibliotece `string.h`.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o tym, jak operować na łańcuchach znaków w języku C, zapoznaj się z poniższymi artykułami:
- https://www.programiz.com/c-programming/c-strings
- https://www.tutorialspoint.com/cprogramming/c_strings.htm
- https://www.tutorialspoint.com/c_standard_library/string_h.htm