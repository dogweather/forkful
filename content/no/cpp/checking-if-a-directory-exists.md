---
title:                "Sjekker om en katalog eksisterer"
html_title:           "C++: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor ?

Å sjekke om en mappe eksisterer er en handling hvor programmet bestemmer om en bestemt sti på datamaskinens filsystem peker på en ekte mappe eller katalog. Programmerere checker dette for å unngå feil som kan oppstå når de prøver å arbeide med en mappe som ikke eksisterer.

## Hvordan?

Her er en grei metode for å sjekke om en mappe eksisterer ved hjelp av eksperimentell filsystembiblioteket i C ++ 17: 

```C++
#include <filesystem>

bool directoryExists(const std::string& path) {
    return std::filesystem::exists(path) && std::filesystem::is_directory(path);
}

int main() {
    std::string path = "/sti/til/mappen/din";

    if (directoryExists(path)) {
        std::cout << "Mappen eksisterer.\n";
    } else {
        std::cout << "Mappen eksisterer ikke.\n";
    }

    return 0;
}
```
Hvis mappen finnes, vil utskriften være "Mappen eksisterer." Ellers vil den være "Mappen eksisterer ikke."

## Dypdykk

Historisk sett, før C++17, måtte programmerere ofte stole på plattformspesifikke systemkall for å sjekke om en mappe eksisterer. Dette kunne gjøre koden mindre bærbar. 

Et alternativ til det eksperimentelle filsystembiblioteket er boost::filesystem, men dette krever at Boost-bibliotekene er installert. 

En del av implementeringsdetaljene å merke seg er at funksjonen `std::filesystem::exists()` sjekker om stien eksisterer i filsystemet, og `std::filesystem::is_directory()` bekrefter at det er en mappe.

## Se også

Her er noen nyttige linker til andre kilder og relatert materiale:
1. [C++ std::filesystem dokumentasjon](http://www.cplusplus.com/reference/filesystem/)
2. [Stackoverflow - Sjekk om mappe eksisterer](https://stackoverflow.com/questions/8233842/how-to-check-if-directory-exist-using-c-and-winapi)
3. [Boost Filesystem Library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)