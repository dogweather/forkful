---
title:                "C: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (ang. Comma-Separated Values) to popularny format pliku używany do przechowywania i przesyłania danych tabelarycznych. W przeciwieństwie do tradycyjnych arkuszy kalkulacyjnych, pliki CSV są łatwe w obsłudze i nie wymagają specjalnego oprogramowania do ich odczytu. Dlatego też, jeśli pracujesz z dużymi ilościami danych, lub potrzebujesz szybko przesłać informacje do innych systemów, z pewnością warto znać podstawy programowania w języku C, aby w pełni wykorzystać możliwości formatu CSV.

## Jak to zrobić

Zanim zaczniemy pisać kod, jako pierwsze musimy zaimportować standardową bibliotekę "stdio.h". Następnie, aby móc działać z plikami, musimy zadeklarować strumień plikowy - to jest po prostu zmienna, która wskazuje na plik. W naszym przypadku będzie to plik CSV, więc musimy użyć funkcji "fopen" z parametrami "nazwa pliku" oraz "tryb", w jakim chcemy otworzyć plik (np. "r", czyli tylko odczyt). Poniżej znajduje się przykład:

```C
#include <stdio.h> 
int main() { 
    FILE *plik = fopen("dane.csv", "r"); 

    // reszta kodu 

    return 0; 
}
```

Kiedy już mamy otwarty plik CSV, możemy odczytać jego zawartość za pomocą funkcji "fscanf". Pierwszym parametrem jest strumień plikowy, kolejnymi są format danych, które chcemy odczytać, a następnie po przecinku umieszczamy zmienne, do których chcemy przypisać odczytane wartości. Przykładowo, jeśli nasz plik CSV wygląda następująco: "Imię,Nazwisko,Wiek", a my chcemy odczytać imię i wiek, to nasz kod może wyglądać tak:

```C
char imie[20]; // zmienna przechowująca imię
int wiek; // zmienna przechowująca wiek

fscanf(plik, "%s,%*s,%d", imie, &wiek); 
// %s oznacza odczytanie ciągu znaków, %*s oznacza pominiecie kolejnego ciągu, a %d oznacza odczytanie liczby całkowitej

printf("Witaj %s, masz %d lat!", imie, wiek);
```

## Głębsza analiza

Powyższy kod jest tylko przykładem bardzo prostego odczytu danych z pliku CSV. W rzeczywistości, przetwarzanie i analiza danych może być znacznie bardziej skomplikowane. Jedną z ważniejszych rzeczy jest obsługa błędów, takich jak brak pliku, nieodpowiedni format lub niekompletne dane. Warto również zapoznać się z innymi formatami plików tabelarycznych, takimi jak JSON, które są coraz bardziej popularne w dzisiejszych czasach.

## Zobacz również

- Dokumentacja języka C: https://en.cppreference.com/w/c
- Poradnik dla początkujących w programowaniu w C: https://www.tutorialspoint.com/cprogramming/index.htm
- Przykładowy kod wykorzystujący pliki CSV w języku C: https://www.sanfoundry.com/c-program-implement-csv-file-parser-using-structure/