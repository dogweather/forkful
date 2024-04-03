---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:37.751488-07:00
description: "Jak to zrobi\u0107: C nie posiada wbudowanych funkcji do bezpo\u015B\
  redniego wyszukiwania oraz zamiany w ci\u0105gach znak\xF3w. Mo\u017Cna jednak tego\
  \ dokona\u0107, \u0142\u0105cz\u0105c r\xF3\u017Cne\u2026"
lastmod: '2024-03-13T22:44:35.869428-06:00'
model: gpt-4-0125-preview
summary: "C nie posiada wbudowanych funkcji do bezpo\u015Bredniego wyszukiwania oraz\
  \ zamiany w ci\u0105gach znak\xF3w."
title: Wyszukiwanie i zamienianie tekstu
weight: 10
---

## Jak to zrobić:
C nie posiada wbudowanych funkcji do bezpośredniego wyszukiwania oraz zamiany w ciągach znaków. Można jednak tego dokonać, łącząc różne dostępne funkcje obsługi ciągów z biblioteki `<string.h>` wraz z pewną logiką własną. Poniżej znajduje się podstawowy przykład, jak wyszukać podciąg w ciągu i go zastąpić. Dla uproszczenia, przykład zakłada wystarczającą wielkość bufora i nie zajmuje się kwestiami alokacji pamięci, które powinny być rozważone w kodzie produkcyjnym.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char bufor[1024];
    char *punkt_wstawienia = &bufor[0];
    const char *tmp = source;
    size_t dl_sub = strlen(sub), dl_nowy_sub = strlen(new_sub);
    size_t dl_do_znalezienia;

    while ((tmp = strstr(tmp, sub))) {
        // Oblicz długość do miejsca znalezienia
        dl_do_znalezienia = tmp - source;
        
        // Kopiuj część przed znalezieniem
        memcpy(punkt_wstawienia, source, dl_do_znalezienia);
        punkt_wstawienia += dl_do_znalezienia;
        
        // Kopiuj nowy podciąg
        memcpy(punkt_wstawienia, new_sub, dl_nowy_sub);
        punkt_wstawienia += dl_nowy_sub;
        
        // Przesuń za znalezione miejsce w ciągu źródłowym
        tmp += dl_sub;
        source = tmp;
    }
    
    // Kopiuj pozostałą część ciągu źródłowego
    strcpy(punkt_wstawienia, source);
    
    // Drukuj zmodyfikowany ciąg
    printf("Zmodyfikowany ciąg: %s\n", bufor);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char nowySub[] = "próba";
    
    replaceSubstring(sourceStr, sub, nowySub);
    
    return 0;
}
```

Przykładowe wyjście:
```
Zmodyfikowany ciąg: Hello, this is a próba. This próba is simple.
```

Ten kod demonstruje prostą metodę wyszukiwania wszystkich wystąpień podciągu (`sub`) w ciągu źródłowym i zastępowania ich innym podciągiem (`nowySub`), używając funkcji `strstr` do znalezienia punktu początkowego każdego dopasowania. Jest to bardzo podstawowy przykład, który nie zajmuje się złożonymi scenariuszami, takimi jak nakładające się na siebie podciągi.

## Dogłębna analiza
Podejście użyte w sekcji "Jak to zrobić" jest podstawowe, ilustrujące, jak osiągnąć wyszukiwanie tekstu i zamianę w C bez żadnych zewnętrznych bibliotek. Historycznie, ze względu na nacisk C na zarządzanie pamięcią na niskim poziomie i wydajność, jego standardowa biblioteka nie obejmuje funkcji wysokiego poziomu manipulacji ciągami, jakie można znaleźć w językach takich jak Python czy JavaScript. Programiści muszą ręcznie zarządzać pamięcią i łączyć różne operacje na ciągach, aby osiągnąć pożądane rezultaty, co zwiększa złożoność, ale oferuje większą kontrolę i efektywność.

Ważne jest, aby zauważyć, że to ręczne podejście może być podatne na błędy, szczególnie przy zarządzaniu alokacjami pamięci i rozmiarami buforów. Nieprawidłowe obsługiwanie może prowadzić do przepełnienia bufora i uszkodzenia pamięci, czyniąc kod podatnym na ryzyka bezpieczeństwa.

W wielu praktycznych scenariuszach, zwłaszcza tych wymagających złożonego przetwarzania tekstów, często warto rozważyć integrację zewnętrznych bibliotek, takich jak PCRE (Perl Compatible Regular Expressions) dla wyszukiwania opartego na wyrażeniach regularnych i zamiany, co może uprościć kod i zmniejszyć potencjał błędów. Ponadto, nowoczesne standardy i kompilatory C coraz częściej oferują wbudowane funkcje i bezpieczniejsze alternatywy do manipulacji ciągami, mając na celu łagodzenie powszechnie obserwowanych problemów w starszych kodach C. Jednakże, fundamentalne zrozumienie ręcznego przetwarzania tekstu pozostaje cenną umiejętnością w arsenale programisty, szczególnie dla optymalizacji aplikacji krytycznych pod względem wydajności.
