---
title:                "C: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista jest pewien, co najmniej raz w życiu, napisał kod, który zawierał różnego rodzaju błędy, takie jak niepotrzebne znaki lub znaki, które nie pasują do naszego zamierzonego wzorca. Aby zoptymalizować nasz kod i uniknąć takich błędów, warto zapoznać się ze sposobami usuwania znaków odpowiadających danemu wzorcowi.

## Jak to zrobić

Jedną z najprostszych metod usuwania znaków odpowiadających danemu wzorcowi jest użycie pętli i funkcji wbudowanej w język C - `strstr ()`. Poniżej przedstawiono przykładowy kod usuwania wszystkich wystąpień danego wzorca z łańcucha:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "To jest przykładowy tekst, w którym będziemy usuwać litery a i b.";
    char pattern[] = "ab";
    char *result;

    while((result = strstr(text, pattern)) != NULL) {
        memmove(result, result + strlen(pattern), strlen(result + strlen(pattern)));
    }
    printf("%s\n", text);
    return 0;
}
```

Kod ten wykorzystuje funkcję `strstr ()`, która znajduje pierwsze wystąpienie danego wzorca w łańcuchu i zwraca wskaźnik na ten fragment tekstu. Następnie, za pomocą funkcji `memmove ()`, usuwane są znaki odpowiadające danemu wzorcowi. Pętla wykonuje się dopóki znaleziony wzorzec nie zostanie usunięty z całego tekstu.

Przykładowy wynik działania tego kodu będzie wyglądać następująco:

`To jest przykłody tekst, w kórym dmy uswa itey er."

## Pogłębione informacje

W języku C istnieje również szereg innych funkcji przydatnych do usuwania znaków odpowiadających danemu wzorcowi, takie jak `strtok ()` czy `strpbrk ()`. Warto również zauważyć, że podczas usuwania znaków, należy pamiętać o odpowiednim alokowaniu pamięci i zwalnianiu jej po zakończeniu działania funkcji.

Zaleca się również, aby zapoznać się z dokumentacją języka C oraz różnymi bibliotekami, które mogą zawierać funkcje ułatwiające usuwanie znaków odpowiadających danemu wzorcowi oraz inne operacje na łańcuchach.

## Zobacz także

- [Dokumentacja języka C](https://en.cppreference.com/w/c)
- [Biblioteka C string](https://en.cppreference.com/w/c/string)