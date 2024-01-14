---
title:    "C: Wydrukowanie danych debugowania"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

W języku C, debugowanie kodu jest jednym z najważniejszych aspektów, które pomagają programistom znaleźć i naprawić błędy. Wypisywanie informacji do konsoli jest nieodłączną częścią procesu debugowania. Udostępnia ono wgląd w przebieg działania kodu i może pomóc w zidentyfikowaniu problemów. Jest to więc niezwykle ważne narzędzie dla programistów, którzy starają się naprawić błędy w swoich programach.

## Jak używać

Istnieje kilka sposobów na wypisywanie debugowych informacji w języku C. Jedną z najpopularniejszych metod jest użycie funkcji `printf` z biblioteki standardowej `stdio.h`. Funkcja ta umożliwia wyświetlanie tekstu, zmiennych oraz wyrażeń na konsoli. Przykładowy kod wygląda następująco:

```C
#include <stdio.h>

int main(){
    int a = 5;
    float b = 3.14;
    char c = 'X';

    printf("Wartość a: %d\n", a);
    printf("Wartość b: %f\n", b);
    printf("Znak c: %c\n", c);

    return 0;
}
```

Output:

```
Wartość a: 5
Wartość b: 3.140000
Znak c: X
```

Jak widać, możemy wypisywać zmienne różnego typu i nawet łączyć je ze stałymi tekstowymi. W przypadku liczb zmiennoprzecinkowych, używamy specjalnego symbolu `%f`, a dla zmiennych typu znakowego `%c`.

Możemy również wstawiać wyrażenia wewnątrz funkcji `printf`, co jest szczególnie przydatne w celu wypisywania wartości złożonych z kilku zmiennych. Przykładowy kod wygląda następująco:

```C
#include <stdio.h>

int main(){
    int a = 5, b = 10;
    printf("Wartość a * b: %d\n", a * b);

    return 0;
}
```

Output:

```
Wartość a * b: 50
```

## Wnikliwe podejście

Wypisywanie debugowych informacji to nie tylko proste wyświetlanie wartości zmiennych na ekranie. Może być również wykorzystywane w bardziej zaawansowany sposób. Na przykład, możemy zaimplementować funkcję `debug` z wykorzystaniem dyrektywy `#define` w celu ułatwienia sobie wypisywania informacji. Przykładowy kod wyglądałby tak:

```C
#include <stdio.h>
#define DEBUG(a) printf("Wartość " #a " : %d\n", a)

int main(){
    int a = 5, b = 10;

    DEBUG(a);
    DEBUG(b);

    return 0;
}
```

Output:

```
Wartość a: 5
Wartość b: 10
```

Możemy również wykorzystać dyrektywę `#ifndef` w celu wyłączenia wyświetlania debugowych informacji w czasie rzeczywistym (np. w produkcji). Przykładowy kod wyglądałby tak:

```C
#include <stdio.h>

#ifdef DEBUG
#define DEBUGMSG(a) printf("DEBUG: Wartość " #a " : %d\n", a)
#else
#define DEBUGMSG(a)
#endif

int main(){
    int a = 5, b = 10;

    DEBUGMSG(a);
    DEBUGMSG(b);

    return 0;
}
```

Output (w przypadku braku dyrektywy `DEBUG`):

```
Brak wyświetlonych informacji debugowych.
```

Należy jednak pamiętać, że nadmierna ilość wypisywanych informacji debugowych może spowolnić nasz program i w niektórych przypadkach może być