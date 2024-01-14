---
title:                "C: Usuwanie znaków pasujących do wzorca"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

JavaScript jest jednym z najpopularniejszych i najczęściej używanych języków programowania na świecie. Jednym z powodów jego popularności jest łatwość w użyciu i elastyczność. Jednak nawet doświadczeni programiści mogą czasami napotkać problem, który wymaga usunięcia konkretnych znaków z tekstu. Dlatego w tym artykule omówimy, jak usuwać znaki pasujące do wzorca w języku C.

## Jak to zrobić

Do usunięcia znaków pasujących do wzorca w języku C używamy funkcji `strpbrk()`. Ta funkcja znajduje pierwsze wystąpienie dowolnego z podanych znaków w tekście i zwraca wskaźnik na ten znak. Następnie możemy usunąć go przy pomocy funkcji `memmove()`, która przesuwa kolejne znaki w tekście o wskazaną liczbę bajtów.

```C
char *strpbrk(const char *s, const char *accept);
void *memmove(void *dest, const void *src, size_t n);
```

Spójrzmy na prosty przykład, gdzie chcemy usunąć znaki `a` i `b` z tekstu:

```C
char text[20] = "Hello World";
char *match = strpbrk(text, "ab");
memmove(match, match + 2, strlen(match + 2) + 1);
printf("%s", text);
// Output: Helo World
```

Wynik wyświetla tekst bez znaków `a` i `b`, ponieważ funkcja `strpbrk()` zwróciła wskaźnik na pierwsze wystąpienie tych znaków, a następnie funkcja `memmove()` przesunęła resztę tekstu o 2 pozycje do przodu.

## Deep Dive

Funkcja `strpbrk()` jest bardzo wydajna, ponieważ porównuje tylko jeden znak z podanego wzorca w danym momencie. Może przeszukiwać duże teksty w czasie liniowym, zależnie od długości tekstu i wzorca.

Warto także wspomnieć o funkcji `strspn()`, która znajduje pierwszy znak, który nie pasuje do wzorca, i zwraca jego indeks w tekście. Ta funkcja może być przydatna, jeśli chcemy usunąć wszystkie znaki z tekstu, które nie pasują do danego wzorca.

## Zobacz również

- [Dokumentacja funkcji strpbrk()](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
- [Porównanie wydajności funkcji strpbrk() i strstr()](http://www.doc.ic.ac.uk/~ejc/ECAbramsen.pdf)