---
title:    "C: Pisanie do standardowego błędu"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego warto pisać do standardowego błędu w języku C

Pisanie do standardowego błędu jest nieodzownym elementem tworzenia niezawodnych programów w języku C. Wszyscy programiści powinni znać tę technikę w celu wykrywania i diagnozowania błędów w swoich programach.

## Jak to zrobić?

Aby pisać do standardowego błędu w języku C, należy użyć funkcji `fprintf()` z biblioteki standardowej `stdio.h`. Funkcja ta przyjmuje dwa argumenty: strumień, do którego ma zostać napisana wiadomość, oraz format wyjściowy wiadomości. W przypadku standardowego błędu, należy podać jako pierwszy argument `stderr`, a jako drugi format wiadomości.

Oto przykładowy kod w języku C wykorzystujący funkcję `fprintf()` do wypisania wiadomości na standardowy błąd:

```C
#include <stdio.h>

int main(){
    int x = 5;
    if(x > 10){
        fprintf(stderr, "x jest większe niż 10\n");
    }
    return 0;
}
```

W rezultacie, gdy zmienna `x` ma wartość większą niż 10, program wypisze wiadomość na standardowym błędzie.

## Głębszy wgląd

Pisanie do standardowego błędu może być szczególnie przydatne w przypadku błędów krytycznych, które wymagają natychmiastowego zatrzymania działania programu. Ponadto, funkcja `fprintf()` jest wysoce konfigurowalna i pozwala na precyzyjne określenie, gdzie i w jaki sposób mają być wypisywane wiadomości o błędach.

W przypadku potrzeby bardziej zaawansowanych funkcjonalności, można również użyć funkcji `perror()`, która dodatkowo wypisze komunikat z błędem systemowym.

## Zobacz również

- [Dokumentacja funkcji `fprintf()`](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Zastosowanie funkcji `fprintf()` w praktyce](https://www.geeksforgeeks.org/error-handling-c-programs/)
- [Dokumentacja funkcji `perror()`](https://www.tutorialspoint.com/c_standard_library/c_function_perror.htm)