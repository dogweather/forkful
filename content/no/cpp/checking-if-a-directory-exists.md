---
title:                "Sjekker om en mappe finnes"
html_title:           "C++: Sjekker om en mappe finnes"
simple_title:         "Sjekker om en mappe finnes"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Når vi programmerer, må vi ofte sjekke om en mappe eksisterer. Dette er en viktig oppgave fordi det lar oss håndtere situasjoner der vi trenger å finne, lese eller skrive filer i en bestemt mappe. Ved å sjekke om en mappe eksisterer, sikrer vi at programmet vårt ikke krasjer eller gir feil når vi prøver å håndtere disse situasjonene.

## Hvordan:
For å sjekke om en mappe eksisterer i C ++, kan vi bruke funksjonen `std::filesystem::exists()`. Denne funksjonen tar inn en sti til mappen og returnerer en bool-verdi som indikerer om mappen eksisterer eller ikke. Her er et eksempel på hvordan vi kan bruke denne funksjonen:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path myPath = "/Users/User/Documents";
    if (std::filesystem::exists(myPath)) {
        std::cout << "Mappen finnes!" << std::endl;
    }
    else {
        std::cout << "Mappen finnes ikke." << std::endl;
    }
    return 0;
}
```

I dette eksempelet har vi en variabel `myPath` som inneholder banen til mappen vi vil sjekke. Vi bruker deretter `if`-setningen til å sjekke om mappen eksisterer ved hjelp av `std::filesystem::exists()`. I tillegg skriver vi ut en melding til konsollen basert på resultatet av vår sjekk.

## Dypdykk:
Mens `std::filesystem::exists()` er den enkleste måten å sjekke om en mappe eksisterer i nyere versjoner av C ++, har det også vært andre måter å gjøre dette på. For eksempel pleide programmerere å bruke `std::experimental::filesystem::exists()` i stedet, men denne funksjonen er nå deprecated.

En annen måte å sjekke etter en mappe er å bruke `stat()`-funksjonen, som returnerer informasjon om en gitt sti, inkludert om den peker til en mappe eller ikke. Dette er imidlertid en mer komplisert tilnærming og brukes ikke så mye i dag.

Når det gjelder implementasjonen av `std::filesystem::exists()`, avhenger det av den underliggende operativsystemet. På Windows er den basert på `GetFileAttributes()`-funksjonen, mens den på Linux bruker `stat()`-funksjonen. Det er viktig å merke seg at selv om denne funksjonen er nyttig, er den ikke garantert å være helt nøyaktig, da en mappe kan bli opprettet eller slettet mens programmet kjører.

## Se også:
- [`std::filesystem::exists()` dokumentasjon](https://en.cppreference.com/w/cpp/filesystem/exists)
- [`std::experimental::filesystem::exists()` dokumentasjon](https://en.cppreference.com/w/cpp/filesystem/experimental/is_directory)
- [stat() funksjonen dokumentasjon](https://linux.die.net/man/2/stat)