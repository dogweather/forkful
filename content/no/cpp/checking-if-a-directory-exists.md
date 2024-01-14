---
title:    "C++: Sjekke om en mappe eksisterer"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med programmering, kan det være viktig å kunne sjekke om en mappe eksisterer eller ikke. Dette kan være nyttig for å sikre at filene du trenger for koden din er tilgjengelige, eller for å håndtere unntakstilstander der mappen ikke finnes.

## Hvordan du gjør det

Sjekk om en mappe eksisterer kan gjøres ved hjelp av en funksjon eller metode i C++. Vi vil bruke følgende kode for å sjekke om en mappe med navnet "mappe" eksisterer eller ikke:

```C++
#include <iostream>
#include <filesystem>
namespace fs = std::filesystem;

int main() {
    fs::path sti = fs::current_path() / "mappe";
    if (fs::exists(sti) && fs::is_directory(sti)) {
        std::cout << "Mappen eksisterer!";
    } else {
        std::cout << "Mappen eksisterer ikke!";
    }
    return 0;
}
```

Dette vil legge til mappenavnet på den nåværende banen, og deretter sjekke om mappen eksisterer og om det faktisk er en mappe. Hvis begge betingelsene er oppfylt, vil du få en melding som sier "Mappen eksisterer!", ellers vil du få "Mappen eksisterer ikke!".

## Dypdykk

La oss se nærmere på funksjonene vi brukte i kodeeksempelet:

- `current_path()` gir oss banen til den nåværende mappen. Vi bruker dette til å legge til mappenavnet for å få stien til mappen vi vil sjekke.
- `exists()` sjekker om en fil eller mappe eksisterer på den angitte banen.
- `is_directory()` sjekker om det på den spesifiserte banen er en mappe.

En annen måte å sjekke om en mappe eksisterer på er å bruke `filesystem::status(sti).type()`, som vil returnere en verdi som informerer om typen fil eller mappe på den angitte banen. Hvis dette er lik `filesystem::file_type::directoy`, betyr det at mappen eksisterer.

## Se også

- Reference dokumentasjon for `std::filesystem` biblioteket: https://en.cppreference.com/w/cpp/filesystem
- Eksempler på håndtering av unntakstilstander når en mappe ikke eksisterer: https://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c
- Ekstra informasjon om filsystemnettverket i C++: https://www.cgpprogramming.com/manual/004_File_I_O/