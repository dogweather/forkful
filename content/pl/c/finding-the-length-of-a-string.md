---
title:    "C: Znajdowanie długości ciągu znaków"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Chęć poznania długości ciągu znakowego jest jedną z podstawowych umiejętności w programowaniu w języku C. Jest to niezbędne do przetwarzania tekstu oraz umożliwia tworzenie dynamicznych programów. Pozwala również na efektywne wykorzystanie pamięci, co jest szczególnie istotne w aplikacjach wymagających dużej wydajności.

# Jak wyznaczyć długość ciągu znakowego

Aby wyznaczyć długość ciągu znakowego w języku C, należy wykorzystać funkcję `strlen()` z biblioteki `string.h`. Przyjmuje ona jako argument wskaźnik do ciągu znakowego i zwraca jego długość.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char name[] = "Jan Kowalski";
    int length = strlen(name);

    printf("Długość imienia i nazwiska: %d\n", length);
}
```

Rezultatem działania tego programu będzie wyświetlenie na ekranie: `Długość imienia i nazwiska: 12`.

# Głębsza analiza

Funkcja `strlen()` w rzeczywistości iteruje przez ciąg znakowy, aż do napotkania znaku zakończenia `\0` (pusty znak). Dlatego też, w przypadku gdy w ciągu występują inne puste znaki poza na końcu, funkcja ta może zwrócić niepoprawną długość. W takim przypadku zaleca się wykorzystanie funkcji `strnlen()`, która przyjmuje jako drugi argument maksymalną ilość znaków do sprawdzenia.

Mając już podstawową wiedzę na temat wyznaczania długości ciągu znakowego w języku C, możemy wykorzystać tę funkcję w swoich programach i tworzyć bardziej skomplikowane działania na tekstach.

# Zobacz również

- [Dokumentacja funkcji `strlen()`](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Inne funkcje z biblioteki `string.h`](https://www.tutorialspoint.com/c_standard_library/string_h.htm)