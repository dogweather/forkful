---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:17.723308-07:00
description: "Jak to zrobi\u0107: C nie posiada wbudowanego wsparcia dla wyj\u0105\
  tk\xF3w, jak niekt\xF3re inne j\u0119zyki. Zamiast tego, opiera si\u0119 na kilku\
  \ konwencjonalnych strategiach\u2026"
lastmod: '2024-03-13T22:44:35.894616-06:00'
model: gpt-4-0125-preview
summary: "C nie posiada wbudowanego wsparcia dla wyj\u0105tk\xF3w, jak niekt\xF3re\
  \ inne j\u0119zyki."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
C nie posiada wbudowanego wsparcia dla wyjątków, jak niektóre inne języki. Zamiast tego, opiera się na kilku konwencjonalnych strategiach obsługi błędów, takich jak zwracanie specjalnych wartości przez funkcje oraz ustawianie globalnych zmiennych, takich jak `errno`.

**Zwracanie specjalnych wartości**

Funkcje mogą sygnalizować błędy, zwracając określoną wartość, która mało prawdopodobne jest aby była prawidłowym wynikiem. Oto przykład z liczbami całkowitymi:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Przypadek błędu
    } else {
        *result = 1.0 / number;
        return 0; // Sukces
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Błąd: Dzielenie przez zero.\n");
    } else {
        printf("Odwrotność wynosi: %f\n", result);
    }
    
    return 0;
}
```

**Wyjście:**
```
Błąd: Dzielenie przez zero.
```

**Sprawdzanie `errno`**

W przypadku funkcji bibliotecznych, zwłaszcza tych, które wchodzą w interakcje z systemem lub OS (jak operacje na plikach), `errno` jest ustawiane gdy wystąpi błąd. Aby go użyć, należy dołączyć `errno.h` oraz sprawdzać `errno` po podejrzanym niepowodzeniu:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Błąd otwarcia pliku: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Wyjście:**
```
Błąd otwarcia pliku: Nie ma takiego pliku lub katalogu
```

## Dogłębna analiza
Historycznie, minimalistyczna konstrukcja języka programowania C wykluczała wbudowany mechanizm obsługi wyjątków, co odzwierciedla jego pochodzenie z programowania systemowego na niskim poziomie, gdzie kluczowe są maksymalna wydajność i kontrola na poziomie sprzętowym. Zamiast tego, C przyjmuje bardziej manualne podejście do obsługi błędów, które pasuje do jego filozofii dawania programistom jak największej kontroli, nawet kosztem wygody.

Chociaż to podejście jest zgodne z celami projektowymi C, może również prowadzić do zwięzłego kodu sprawdzającego błędy i potencjalnego pominięcia kontroli błędów, co nowoczesne języki adresują za pomocą strukturalnych mechanizmów obsługi wyjątków. Na przykład, wyjątki w językach takich jak Java czy C# pozwalają na scentralizowane przetwarzanie błędów, czyniąc kod czyściejszym i zarządzanie błędami bardziej bezpośrednie. Jednakże, wyjątki wprowadzają własne obciążenie i złożoność, które mogą nie być idealne dla programowania na poziomie systemowym, gdzie C błyszczy.

Pomimo swojej surowości, ta manualna obsługa błędów w C poinformowała projekt zarządzania błędami w wielu innych językach, oferując model, gdzie wyraźność warunków błędów może prowadzić do bardziej przewidywalnego i łatwiejszego do debugowania kodu. Dla krytycznych systemów, gdzie awarie muszą być zarządzane z gracją, paradygmat obsługi błędów w C—w połączeniu z nowoczesnymi najlepszymi praktykami, takimi jak biblioteki obsługi błędów i konwencje—zapewnia solidność i niezawodność.
