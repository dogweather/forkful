---
title:                "Refaktoryzacja"
date:                  2024-01-26T01:16:46.107355-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego bez zmieniania jego zewnętrznego zachowania. Programiści robią to, aby poprawić czytelność, zmniejszyć złożoność lub sprawić, że kod będzie łatwiejszy w utrzymaniu i skalowaniu, co może zaoszczędzić mnóstwo czasu i bólów głowy w przyszłości.

## Jak to zrobić:
Odświeżmy trochę kodu. Wyobraź sobie, że masz funkcję, która oblicza średnią z liczb całkowitych w tablicy. Na pierwszy rzut oka to trochę skomplikowany bałagan.

**Przed refaktoryzacją:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Sumowanie w warunku pętli for, auć!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Średnia: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Po refaktoryzacji:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Średnia: %f\n", calculateAverage(array, length));
    return 0;
}
```
Nawet na tym prostym przykładzie widać, jak podział funkcji sprawia, że kod jest czystszy i łatwiejszy w utrzymaniu. Każda funkcja ma teraz pojedyncze zadanie – kluczową zasadę w pisaniu przejrzystego kodu.

## Głębsze spojrzenie
Termin "refaktoryzacja" zyskał popularność pod koniec lat 90., szczególnie po publikacji książki Martina Fowlera "Refaktoryzacja: Ulepszanie struktury istniejącego kodu". Refaktoryzacja nie oznacza naprawiania błędów czy dodawania nowych funkcji, lecz polega na ulepszaniu struktury kodu.

Istnieje wiele znakomitych narzędzi i środowisk programistycznych (IDE - Integrated Development Environments) wspomagających proces refaktoryzacji, takich jak CLion dla C i C++, ale zrozumienie tego, co dzieje się za kulisami, pozostaje kluczowe.

Alternatywami dla refaktoryzacji mogą być przepisanie kodu od nowa (ryzykowne i często niepotrzebne) lub życie z długiem technicznym (co może być droższe w dłuższej perspektywie). Szczegóły implementacji różnią się w zależności od projektu, ale do wspólnych refaktoryzacji należy zmiana nazw zmiennych dla większej jasności, dzielenie dużych funkcji na mniejsze i zastępowanie magicznych liczb nazwanymi stałymi.

Ponadto, wzorce takie jak DRY (Don't Repeat Yourself) i zasady SOLID mogą prowadzić twoją podróż refaktoryzacyjną, dążąc do bazy kodu, która jest łatwiejsza do testowania, zrozumienia i współpracy.

## Zobacz również
Aby zagłębić się w świat refaktoryzacji, zerknij na:

- Strona domowa Martina Fowlera: https://martinfowler.com/ z bogactwem artykułów i zasobów na temat refaktoryzacji i projektowania oprogramowania.
- Refactoring.com: https://refactoring.com/ oferuje przykłady i katalogi technik refaktoryzacji.
- Książka "Refaktoryzacja": Uważana za biblię refaktoryzacji, jej przeczytanie daje kompletny obraz metodologii.
- "Czysty kod: Podręcznik dobrego rzemieślnika oprogramowania" autorstwa Roberta C. Martina, który omawia pisanie kodu, który jest łatwy do zrozumienia i utrzymania.
