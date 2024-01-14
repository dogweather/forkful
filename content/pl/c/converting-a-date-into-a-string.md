---
title:                "C: Konwersja daty na ciąg znaków"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Konwersja daty na łańcuch znaków jest bardzo często spotykanym zadaniem w programowaniu. Może być wykorzystywane do wyświetlania dat na stronach internetowych, generowania raportów lub wyświetlania danych dla użytkowników. W tym artykule dowiesz się, jak dokonać tej konwersji w języku C.

# Jak to zrobić

Aby skonwertować datę na łańcuch znaków w języku C, musimy użyć funkcji "strftime". Przyjmie ona dwa argumenty - pierwszy to łańcuch znaków z formatem daty, a drugi to struktura zawierająca datę. Poniżej znajduje się przykładowy kod, który pokazuje, jak to zrobić:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t czas = time(NULL);
    struct tm *aktualny_czas = localtime(&czas);
    char wynik[64];

    strftime(wynik, sizeof wynik, "%A, %e %B %Y", aktualny_czas);
    printf("Dzisiaj jest %s\n", wynik);

    return 0;
}
```

W powyższym przykładzie funkcja "strftime" używa formatu "%A, %e %B %Y" do konwersji daty na przyjazny dla użytkownika łańcuch znaków. Jeżeli uruchomisz ten kod, powinieneś otrzymać wynik "Dzisiaj jest poniedziałek, 22 kwietnia 2019".

Oczywiście, istnieje wiele różnych formatów daty, z których można skorzystać. Poniżej przedstawione są niektóre z najczęściej używanych:

- %d - dzień miesiąca w postaci liczby dziesiętnej (np. 01 lub 31)
- %m - miesiąc w postaci liczby dziesiętnej (np. 04 lub 12)
- %Y - rok w czterocyfrowym formacie (np. 2019)
- %y - rok w dwucyfrowym formacie (np. 19)
- %H - godzina w formacie 24-godzinnym (np. 13 lub 23)
- %I - godzina w formacie 12-godzinnym (np. 01 lub 11)
- %M - minuty (np. 05 lub 45)
- %S - sekundy (np. 02 lub 59)
- %a - skrót nazwy dnia tygodnia (np. pon lub sob)
- %A - nazwa dnia tygodnia (np. poniedziałek lub sobota)
- %b - skrót nazwy miesiąca (np. kwi lub gru)
- %B - nazwa miesiąca (np. kwiecień lub grudzień)

Możesz również stosować różne kombinacje tych formatów w celu uzyskania pożądanego wyniku. Na przykład "%d/%m/%Y" spowoduje, że data zostanie wyświetlona w formacie "dd/mm/rrrr".

# Deep Dive (Głębszy Wgląd)

W języku C istnieje również funkcja "strptime", która działa odwrotnie do funkcji "strftime" - przekonwertuje łańcuch znaków na datę. Wymaga ona trzech argumentów - pierwszy to łańcuch znaków z datą, drugi to format daty, a trzeci to wskaźnik na strukturę, do której zostanie zapisana data. Poniżej przykład użycia tej funkcji:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    char data[] = "22/04/2019";
    struct tm aktualny_czas;

    strptime(data, "%d/%m/%Y", &aktualny_czas);

    printf("Dzisiaj jest %s\n", asctime(&aktualny_czas));

    return 0;
}
```

W powyż