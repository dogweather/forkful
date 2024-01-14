---
title:    "C: Pisanie do standardowego błędu"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Zapewne wiesz, że w języku C możemy korzystać z funkcji `printf` do wyświetlania informacji na standardowym wyjściu. Ale czy wiesz, że istnieje również standardowe wyjście błędów, które jest równie ważne? W tym artykule dowiesz się dlaczego warto pisać do standardowego wyjścia błędów.

## Jak to zrobić

Aby wypisać informację na standardowym wyjściu błędów, możemy skorzystać z funkcji `fprintf` w następujący sposób:

```C
#include <stdio.h>
int main(){
  fprintf(stderr, "To jest komunikat błędu!");
}
```

Wywołanie tej funkcji spowoduje, że komunikat zostanie wypisany na standardowym wyjściu błędów. Możemy również skorzystać ze strumienia `stderr` bezpośrednio, zamiast wywoływać funkcję `fprintf`:

```C
#include <stdio.h>
int main(){
  fprintf(stderr, "To jest komunikat błędu!");
}
```

Pamiętaj, że standardowe wyjście błędów jest wykorzystywane do wypisywania informacji o błędach, dlatego warto używać go w przypadku napotkania problemów w naszym programie.

## Głębszy wgląd

Standardowe wyjście błędów jest bardzo ważne zwłaszcza w przypadku pisania programów, które będziemy uruchamiać z poziomu powłoki. Dzięki temu, że informacje o błędach są wypisywane na oddzielnym wyjściu, możemy łatwiej je znaleźć i zidentyfikować problem.

Dodatkowo, standardowe wyjście błędów jest również wykorzystywane w przypadku pisania skryptów powłoki, gdzie możemy przekierować informacje o błędach do innego pliku lub ich całkowicie zignorować.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o standardowym wyjściu błędów, zapoznaj się z poniższymi artykułami:

- https://pl.wikipedia.org/wiki/Standardowe_wejście_i_wyjście
- https://www.programiz.com/c-programming/c-file-input-output
- https://www.geeksforgeeks.org/error-handling-c-programs/