---
title:                "C++: Sjekke om en mappe eksisterer."
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Enten du er en erfaren C++ utvikler eller en nybegynner, kan det være nyttig å vite hvordan du sjekker om en mappe eksisterer i ditt program. Dette gjør det mulig å utføre forskjellige oppgaver, som å navigere til en bestemt mappe eller sjekke om en fil er på riktig plassering.

# Hvordan gjøre det

Det å sjekke om en mappe eksisterer i C++ krever bruk av en innebygd funksjon som heter `opendir()`. Denne funksjonen tar inn en tekststreng som representerer mappen du ønsker å sjekke. Her er et eksempel på hvordan dette kan gjøres:

```C++
// inkluder nødvendige biblioteker
#include <iostream>
#include <dirent.h>
using namespace std;

int main() {
    // definer mappen som skal sjekkes
    string mappe = "/bruker/navn/program/";

    // forsøk å åpne mappen og lagre returverdien
    DIR* retur = opendir(mappe.c_str());

    // sjekk om returverdien er lik NULL
    if (retur == NULL) {
        // mappen eksisterer ikke, gi brukeren en feilmelding
        cout << "Mappen eksisterer ikke!" << endl;
    } else {
        // mappen eksisterer, gi brukeren en bekreftelse
        cout << "Mappen eksisterer!" << endl;
        // husk å lukke mappen etter at du er ferdig med å sjekke
        closedir(retur);
    }
    
    return 0;
}
```

Koden vil forsøke å åpne mappen som er definert i `mappe` variabelen. Hvis mappen ikke eksisterer, vil `opendir()` returnere en NULL-verdi, som betyr at vi kan gi en feilmelding til brukeren. Ellers, hvis mappen eksisterer, vil vi få en bekreftelse og lukke mappen igjen.

## Dypdykk

Når du sjekker om en mappe eksisterer, kan det være nyttig å vite at `opendir()` funksjonen også tar inn flere argumenter. For eksempel, hvis du kun ønsker å sjekke om mappen eksisterer og ikke åpne den, kan du bruke `access()` funksjonen i stedet. Her er et eksempel på hvordan det kan gjøres:

```C++
// sjekk om mappen eksisterer uten å åpne den
if (access(mappe.c_str(), F_OK) == 0) {
    // mappen eksisterer, gi brukeren en bekreftelse
    cout << "Mappen eksisterer!" << endl;
} else {
    // mappen eksisterer ikke, gi brukeren en feilmelding
    cout << "Mappen eksisterer ikke!" << endl;
}
```

Her bruker vi `access()` funksjonen som tar inn to argumenter: en tekststreng som representerer mappen, og en funksjonell modus som forteller hvilken operasjon som skal utføres på mappen. `F_OK` modus betyr at vi kun ønsker å sjekke om mappen eksisterer.

# Se også

- [C++ Dokumentasjon: opendir()](https://www.cplusplus.com/reference/cstdio/opendir/)
- [Programiz: How to Check if a Directory Exists in C++](https://www.programiz.com/cpp-programming/files-directories-check-existence)