---
title:                "Uttrekking av understrenger"
html_title:           "C++: Uttrekking av understrenger"
simple_title:         "Uttrekking av understrenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Å ekstrahere substringer er nyttig når du har en lengre streng og bare trenger en del av den. Dette kan være nyttig når du ønsker å manipulere eller analysere en mindre del av teksten.

## Hvordan
Her viser vi deg hvordan du kan ekstrahere substringer ved hjelp av C++.

```C++
// Definerer en streng
string tekst = "Denne setningen er litt for lang.";
// Ekstraherer substring fra indeks 6 til 19, inkludert
string substring = tekst.substr(6, 14);

// Skriver ut substring
cout << "Substring: " << substring << endl;
// Output: Substring: setningen er

// Ekstraherer fra starten av strengen til indeks 10
string start = tekst.substr(0, 10);

// Skriver ut starten av strengen
cout << "Start: " << start << endl;
// Output: Start: Denne setn

// Ekstraherer fra indeks 20 til slutten av strengen
string slutt = tekst.substr(20);

// Skriver ut slutten av strengen
cout << "Slutt: " << slutt << endl;
// Output: Slutt: litt for lang.
```

## Deep Dive
Funksjonen substr() i C++ lar deg definere en startindeks og lengden på substringen du vil ekstrahere. Dersom lengden ikke blir spesifisert, vil den ekstraherte substringen bestå av resten av strengen fra startindeksen.
Det finnes også en annen versjon av substr() som tar imot en enkel integer som argument for startindeksen og returnerer resten av strengen fra denne indeksen. Dette kan være nyttig dersom du kun kjenner startindeksen, men ikke lengden på substringsen du vil ekstrahere.

## Se Også
- [C++ string class reference](http://www.cplusplus.com/reference/string/string/)
- [Getting substrings in C++](https://www.geeksforgeeks.org/getline-string-c/)
- [Manipulating strings in C++](https://www.freecodecamp.org/news/how-to-manipulate-strings-in-c/)