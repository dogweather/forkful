---
title:                "Praca z plikami csv"
html_title:           "C++: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pracowanie z plikami CSV może być częstym zadaniem dla programistów. CSV, czyli plik Wartości Rozdzielane Przecinkami, jest sposobem na przechowywanie danych w formacie tabelarycznym, gdzie każda linijka odpowiada wierszowi, a wartości są rozdzielane przecinkami. Programiści korzystają z CSV, ponieważ jest to prosty i zrozumiały sposób przechowywania danych, co ułatwia późniejszą obróbkę i analizę.

## Jak to zrobić:
Aby pracować z plikami CSV w języku C++, potrzebujemy wczytać odpowiednią bibliotekę. Możemy to zrobić w następujący sposób:
```
#include <fstream> 
#include <iostream>
#include <sstream> 
#include <string> 
#include <vector> 
```

Następnie, aby wczytać plik CSV, musimy utworzyć zmienną typu ```ifstream``` i przekazać do niej ścieżkę do pliku, który chcemy wczytać. Następnie, używając pętli, możemy odczytać każdą linijkę i przekonwertować jej zawartość do wektora. Spójrzmy na przykład:
```
ifstream plik("dane.csv"); 

string linia; 
vector<vector<string>> dane; 
while(getline(plik, linia)) { 
    stringstream ss(linia); 
    vector<string> linia_danych; 

    string wartosc; 
    while(getline(ss, wartosc, ',')) { 
        linia_danych.push_back(wartosc); 
    } 
    dane.push_back(linia_danych); 
} 
```
Teraz możemy wyświetlić dane i przetwarzać je według potrzeb.

## Głębszy zanurzenie:
Pierwsze implementacje plików CSV pojawiły się w latach 70. w programie VisiCalc, a później zyskały popularność w Excelu. Alternatywą dla CSV są bazy danych, ale pliki te są nadal wykorzystywane przez programistów ze względu na swoją prostotę. W C++ istnieje również wiele bibliotek do pracy z CSV, np. Boost CSV.

##Zobacz też:
- Dokumentacja biblioteki Boost CSV: https://www.boost.org/doc/libs/1_51_0/libs/serialization/doc/unsupported.html#csv
- Poradnik dla początkujących o pracy z plikami CSV w C++: https://thispointer.com/basics-of-boost-c-sqlite-tutorial-insert-values-in-employee-table/
- Przykładowy plik CSV: https://raw.githubusercontent.com/jdolan/quetoo/master/misc/example.csv