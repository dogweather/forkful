---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:37.775217-07:00
description: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery w j\u0119\
  zyku C polega na przekszta\u0142ceniu wszystkich wielkich liter w danym \u0142a\u0144\
  cuchu na odpowiadaj\u0105ce im ma\u0142e litery.\u2026"
lastmod: 2024-02-19 22:04:55.048360
model: gpt-4-0125-preview
summary: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery w j\u0119zyku\
  \ C polega na przekszta\u0142ceniu wszystkich wielkich liter w danym \u0142a\u0144\
  cuchu na odpowiadaj\u0105ce im ma\u0142e litery.\u2026"
title: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja łańcucha znaków na małe litery w języku C polega na przekształceniu wszystkich wielkich liter w danym łańcuchu na odpowiadające im małe litery. Programiści często wykonują tę operację, aby ujednolicić wejściowy tekst pod kątem porównań, operacji wyszukiwania, lub po prostu dla estetycznej spójności wyjścia.

## Jak to zrobić:

C nie ma wbudowanej funkcji bezpośrednio konwertującej łańcuch znaków na małe litery, w przeciwieństwie do niektórych języków wyższego poziomu. Jednak proces ten można łatwo zaimplementować przy użyciu funkcji standardowej biblioteki C. Poniżej znajduje się przewodnik krok po kroku oraz przykład ilustrujący, jak przekonwertować łańcuch znaków na małe litery.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Oryginał: %s\n", text);

    toLowerCase(text);
    printf("Małe litery: %s\n", text);

    return 0;
}
```

**Przykładowe wyjście:**

```
Oryginał: Hello, World!
Małe litery: hello, world!
```

W tym przykładzie funkcja `toLowerCase` iteruje przez każdy znak wejściowego łańcucha, konwertując go na odpowiadającą mu małą literę przy użyciu funkcji `tolower` z `ctype.h`. Modyfikacja odbywa się w miejscu, zmieniając oryginalny łańcuch.

## Dogłębna analiza

Funkcja `tolower` używana w powyższym przykładzie jest częścią standardowej biblioteki C, konkretnie w pliku nagłówkowym `ctype.h`. Działa ona na podstawie bieżącej lokalizacji, ale dla standardowej lokalizacji "C" obsługuje zestaw znaków ASCII, gdzie litery od 'A' do 'Z' są konwertowane na 'a' do 'z'.

Historycznie, obsługa kodowania znaków i konwersji wielkości liter w C była ściśle związana z zestawem znaków ASCII, co ograniczało jej użyteczność w aplikacjach międzynarodowych lub lokalizowanych, gdzie powszechne są znaki spoza zestawu ASCII. Nowoczesne języki programowania mogą oferować wbudowane metody łańcuchowe do wykonywania konwersji przypadków, biorąc pod uwagę lokalizację i znaki Unicode, czego C nie posiada natywnie.

W scenariuszach wymagających rozległej manipulacji tekstem, szczególnie z znakami spoza ASCII, programiści mogą rozważyć użycie bibliotek oferujących lepsze wsparcie internacjonalizacji, takich jak ICU (International Components for Unicode). Jednak dla większości aplikacji radzących sobie z tekstem ASCII, przedstawione podejście jest efektywne i proste. Podkreśla to skłonność C do dawania programistom kontroli nad manipulacją danymi, choć wymaga to nieco więcej pracy w porównaniu z językami wyższego poziomu.
