---
title:                "Praca z plikami csv"
html_title:           "C: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Co to jest & dlaczego to robimy?
Pracowanie z plikami CSV to nieodłączna część codzienności programisty. CSV to skrót od "Comma-Separated Values", czyli pliku przechowującego dane w formie tabeli z wartościami oddzielonymi przecinkami. Dlaczego warto pracować z CSV? Bardzo często dane są zapisywane w tym formacie, przez co praca z nimi jest niezbędna dla większości projektów programistycznych.

## Jak to zrobić:
Kodowanie z wykorzystaniem CSV w języku C jest bardzo proste i intuicyjne. Przykładowy kod wygląda następująco:
```
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int main() {
   FILE *fp;
   char line[100];
   fp = fopen("plik.csv" , "r");

   while(fgets(line, 100, fp) != NULL) {
      char *token = strtok(line, ",");
      while(token != NULL) {
         printf("%s ", token);
         token = strtok(NULL, ",");
      }
      printf("\n");
   }
   fclose(fp);
   return 0;
}
```
Powyższy kod odczytuje plik CSV o nazwie "plik.csv" i wypisuje jego zawartość na ekran. Za każdym razem, kiedy pobiera kolejną linię, dzieli ją na poszczególne kolumny za pomocą funkcji ```strtok``` i drukuje je na ekran. Proces ten powtarza się aż do wyczerpania zawartości pliku.

## Głębszy zanurzenie:
Pierwsza wersja pliku CSV pojawiła się w 1972 roku i została stworzona przez Petera F. Petersona. Początkowo był to nieoficjalny standard używany tylko w niektórych programach, jednak w 1987 roku został oficjalnie uznany przez RFC 4180. Alternatywnym formatem przechowywania danych może być na przykład XML, jednak CSV jest często wybierany ze względu na swoją prostotę i czytelność dla człowieka. Implementacja wczytywania danych z pliku CSV jest także możliwa za pomocą funkcji ```fscanf``` lub biblioteki libcsv.

## Zobacz też:
Jeśli chcesz dowiedzieć się więcej o standardzie CSV, polecam przeczytać oficjalny dokument RFC 4180: https://tools.ietf.org/html/rfc4180. Jeśli natomiast wolisz skorzystać z biblioteki libcsv, znajdziesz ją tutaj: https://github.com/zaiah-libc/libcsv.