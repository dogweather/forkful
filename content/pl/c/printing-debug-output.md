---
title:                "C: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie komunikatów debugowania jest niezbędnym elementem procesu programowania. Pozwala nam to na śledzenie i analizę działania naszego kodu, a także na szybkie znalezienie ewentualnych błędów. Oczywiście przydatne jest jedynie w fazie developmentu i nie powinno znajdować się w finalnej wersji programu, jednak może znacząco ułatwić nam pracę nad projektem.

## Jak To Zrobić

Aby wyświetlić komunikaty debugowania w języku C, musimy użyć funkcji ```printf```. Jest to podstawowa funkcja do wyświetlania informacji w konsoli. Możemy umieścić ją w dowolnym miejscu naszego kodu, gdzie chcemy uzyskać informacje o wartościach zmiennych, przebiegu pętli itp. Przykładowy kod wyglądałby następująco:

```
#include <stdio.h>

int main()
{
    int x = 5;
    printf("Wartość zmiennej x wynosi: %d", x);
    return 0;
}
```

Po uruchomieniu tego kodu, w konsoli zostanie wyświetlona informacja: "Wartość zmiennej x wynosi: 5". Dodatkowo, możemy również wyświetlać więcej niż jedną zmienną w jednym wywołaniu funkcji ```printf```, na przykład:

```
printf("Wartości zmiennych to: x = %d, y = %f, z = %c", x, y, z);
```

W tym przypadku, musimy pamiętać o odpowiedniej kolejności zmiennych w funkcji.

## Głębszy Rzut Oka

Komunikaty debugowania mogą również zawierać bardziej szczegółowe informacje, takie jak wartości zmiennych po każdej iteracji pętli lub komunikaty o błędach. W takim przypadku, możemy użyć funkcji ```fprintf```, która oprócz wyświetlania komunikatów w konsoli, umożliwia również zapisywanie ich do pliku. Przykładowy kod wyglądałby tak:

```
#include <stdio.h>

int main()
{
    FILE *fp;
    
    fp = fopen("debug.txt", "w");
    fprintf(fp, "Wartość zmiennej x wynosi: %d", x);

    return 0;
}
```

Taka metoda może być przydatna w przypadku, gdy chcemy zachować historyczne informacje o przebiegu działania naszego programu.

## Zobacz Również

- [CppSho