---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:49.480327-07:00
description: "W \u015Bwiecie programowania praca z plikami CSV (Comma-Separated Values)\
  \ obejmuje odczyt z plik\xF3w tekstowych oraz zapis danych do nich, zorganizowanych\
  \ w\u2026"
lastmod: '2024-03-13T22:44:35.910787-06:00'
model: gpt-4-0125-preview
summary: "W \u015Bwiecie programowania praca z plikami CSV (Comma-Separated Values)\
  \ obejmuje odczyt z plik\xF3w tekstowych oraz zapis danych do nich, zorganizowanych\
  \ w wiersze, gdzie ka\u017Cdy wiersz reprezentuje rekord, a poszczeg\xF3lne pola\
  \ rekordu s\u0105 oddzielone przecinkami."
title: Praca z plikami CSV
weight: 37
---

## Co i dlaczego?

W świecie programowania praca z plikami CSV (Comma-Separated Values) obejmuje odczyt z plików tekstowych oraz zapis danych do nich, zorganizowanych w wiersze, gdzie każdy wiersz reprezentuje rekord, a poszczególne pola rekordu są oddzielone przecinkami. Programiści manipulują plikami CSV dla łatwości importu/eksportu danych między różnymi systemami, ze względu na ich powszechne wsparcie i prostotę przechowywania danych tablicowych.

## Jak:

### Odczytywanie plików CSV
Aby odczytać plik CSV w języku C, używamy standardowych funkcji wejścia/wyjścia plików wraz z funkcjami manipulacji ciągami znaków do parsowania każdej linii. Poniżej znajduje się podstawowy przykład odczytu pliku CSV i wyświetlania pól każdego wiersza na konsoli.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("dane.csv", "r");
    if (!fp) {
        printf("Nie można otworzyć pliku\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *pole = strtok(buf, ",");
        while(pole) {
            printf("%s\n", pole);
            pole = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Przykładowy `dane.csv`:
```
Imię,Wiek,Zawód
John Doe,29,Inżynier oprogramowania
```

Przykładowy wynik:
```
Imię
Wiek
Zawód
John Doe
29
Inżynier oprogramowania
```

### Zapisywanie do plików CSV
Podobnie, zapis do pliku CSV wiąże się z użyciem `fprintf` do zapisu danych w formacie oddzielonym przecinkami.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("wynik.csv", "w");
    if (!fp) {
        printf("Nie można otworzyć pliku\n");
        return 1;
    }

    char *nagłówki[] = {"Imię", "Wiek", "Zawód", NULL};
    for (int i = 0; nagłówki[i] != NULL; i++) {
        fprintf(fp, "%s%s", nagłówki[i], (nagłówki[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Naukowiec danych");

    fclose(fp);
    return 0;
}
```

Przykładowa zawartość `wynik.csv`:
```
Imię,Wiek,Zawód
Jane Doe,27,Naukowiec danych
```

## Szczegółowa analiza

Format CSV, mimo że wydaje się prosty, kryje w sobie niuanse, takie jak obsługa przecinków w polach oraz otaczanie pól cudzysłowami. Pokazane przykładowe przykłady nie uwzględniają takich złożoności, ani nie obsługują błędów w sposób niezawodny.

Historycznie rzecz biorąc, obsługa plików CSV w C była w dużej mierze ręczna ze względu na niskopoziomową naturę języka oraz brak wbudowanych wysokopoziomowych abstrakcji dla takich zadań. Ta ręczna obsługa obejmuje otwieranie plików, czytanie linii, dzielenie ciągów znaków i konwertowanie typów danych w razie potrzeby.

Chociaż bezpośrednia manipulacja plikami CSV w C dostarcza cenne doświadczenia z zakresu wejścia/wyjścia plików i obsługi ciągów znaków, istnieje kilka nowoczesnych alternatyw obiecujących większą wydajność i mniej podatność na błędy. Biblioteki takie jak `libcsv` i `csv-parser` oferują kompleksowe funkcje do odczytu i zapisu plików CSV, w tym wsparcie dla pól otoczonych cudzysłowami i niestandardowych separatorów.

Alternatywnie, przy pracy w ekosystemach, które to wspierają, integracja z językami lub platformami oferującymi wysokopoziomowe funkcje manipulacji plikami CSV (takie jak Python z biblioteką `pandas`) może być bardziej produktywną drogą dla aplikacji wymagających intensywnej obróbki plików CSV. To podejście międzyjęzykowe wykorzystuje wydajność C oraz możliwości programowania systemowego, przy jednoczesnym wykorzystaniu łatwości użytkowania z innych języków do konkretnych zadań, takich jak obsługa CSV.
