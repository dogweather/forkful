---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:10.116642-07:00
description: "Jak to zrobi\u0107: W C, \u0142a\u0144cuchy znak\xF3w s\u0105 tablicami\
  \ znak\xF3w ko\u0144cz\u0105cymi si\u0119 znakiem null (`\\0`). W przeciwie\u0144\
  stwie do j\u0119zyk\xF3w wy\u017Cszego poziomu, C nie zapewnia\u2026"
lastmod: '2024-03-13T22:44:35.877427-06:00'
model: gpt-4-0125-preview
summary: "W C, \u0142a\u0144cuchy znak\xF3w s\u0105 tablicami znak\xF3w ko\u0144cz\u0105\
  cymi si\u0119 znakiem null (`\\0`)."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## Jak to zrobić:
W C, łańcuchy znaków są tablicami znaków kończącymi się znakiem null (`\0`). W przeciwieństwie do języków wyższego poziomu, C nie zapewnia wbudowanej funkcji do konkatenacji łańcuchów. Zamiast tego używa się funkcji `strcat()` lub `strncat()` z biblioteki `<string.h>`.

Oto prosty przykład użycia `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Cześć, ";
    char source[] = "Świat!";

    strcat(destination, source);

    printf("%s\n", destination);  // Wynik: Cześć, Świat!
    return 0;
}
```

Funkcja `strcat()` przyjmuje dwa argumenty: łańcuch docelowy (który musi mieć wystarczająco dużo miejsca, aby pomieścić skonkatenowany wynik) oraz łańcuch źródłowy. Następnie dołącza łańcuch źródłowy do łańcucha docelowego.

Dla większej kontroli nad ilością konkatenowanych znaków, bezpieczniejsze jest użycie `strncat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Cześć, ";
    char source[] = "Świat!";
    int num = 3; // Liczba znaków do dodania

    strncat(destination, source, num);

    printf("%s\n", destination);  // Wynik: Cześć, Świ
    return 0;
}
```

To ogranicza konkatenację do pierwszych `num` znaków łańcucha źródłowego, pomagając zapobiec przekroczeniom bufora.

## Dogłębna analiza
Funkcje `strcat()` i `strncat()` są częścią standardowej biblioteki C od jej powstania, odzwierciedlając niskopoziomową naturę języka, która wymaga ręcznego zarządzania łańcuchami znaków i pamięcią. W przeciwieństwie do wielu nowoczesnych języków programowania, które traktują łańcuchy znaków jako obiekty pierwszej klasy z wbudowanymi operatorami konkatenacji (takimi jak `+` czy `.concat()`), podejście C wymaga bardziej dogłębnego zrozumienia wskaźników, alokacji pamięci i potencjalnych pułapek, takich jak przekroczenia bufora.

Chociaż `strcat()` oraz `strncat()` są szeroko stosowane, często krytykuje się je za potencjalne tworzenie luk bezpieczeństwa, jeśli nie są używane ostrożnie. Przekroczenia bufora, kiedy dane przekraczają przydzieloną pamięć, mogą prowadzić do awarii lub być wykorzystywane do wykonania dowolnego kodu. W rezultacie programiści coraz częściej zwracają się ku bezpieczniejszym alternatywom, takim jak `snprintf()`, które zapewniają bardziej przewidywalne zachowanie, ograniczając liczbę znaków zapisanych do łańcucha docelowego w oparciu o jego rozmiar:

```c
char destination[50] = "Cześć, ";
char source[] = "Świat!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

Ta metoda jest bardziej rozwlekła, ale znacznie bezpieczniejsza, co podkreśla zmianę w praktykach programowania w C w kierunku priorytetowego traktowania bezpieczeństwa i niezawodności kosztem zwięzłości.

Pomimo tych wyzwań, konkatenacja łańcuchów znaków w C jest fundamentalną umiejętnością, kluczową dla skutecznego programowania w tym języku. Zrozumienie jej niuansów i związanych z nią ryzyk jest kluczowe do opanowania programowania w C.
