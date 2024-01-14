---
title:                "C++: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en vanlig oppgave for programmerere, spesielt i C++. Tekstfiler lar deg lagre og manipulere tekstbasert data på en enkel og effektiv måte. Enten du trenger å lagre brukerinnstillinger eller behandle store deler av tekst, er tekstfiler et nyttig verktøy å ha i verktøykassen din.

## Hvordan

For å skrive en tekstfil i C++, må du følge disse trinnene:

1. Åpne en ny `ofstream`-strøm ved hjelp av `open()`-funksjonen og angi filnavnet du vil bruke.

````C++
ofstream utstrøm("tekstfil.txt");
````

2. Skriv til tekstfilen ved å bruke strømoperatoren `<<`.

````C++
utstrøm << "Dette er en eksempeltekst som vil bli lagret i tekstfilen." << endl;
````

3. Når du er ferdig med å skrive til filen, må du lukke strømmen ved hjelp av `close()`-funksjonen.

````C++
utstrøm.close();
````

## Dykk dypere

Det er viktig å merke seg at når du åpner en `ofstream`-strøm, vil den opprette en ny fil hvis filen ikke allerede eksisterer. Hvis filen allerede eksisterer, vil den bli overskrevet. Dette kan være problematisk hvis du ikke ønsker å miste data som allerede er lagret i filen. I så fall kan du bruke `app`-flagget sammen med `open()`-funksjonen for å åpne filen i append-modus, noe som betyr at nytt innhold vil bli lagt til på slutten av filen i stedet for å bli overskrevet.

````C++
ofstream utstrøm;
utstrøm.open("tekstfil.txt", ios::app);
````

Det er også viktig å lukke strømmen når du er ferdig med å bruke den. Dette sikrer at eventuelle ufullstendige operasjoner blir fullført og at ressurser blir frigjort.

## Se også

- [C++ - Grundleggende om filbehandling](https://www.w3schools.com/cpp/cpp_files.asp)
- [C++ - ofstream](https://www.geeksforgeeks.org/ofstream-objects-in-cpp/)
- [Hvordan bruke Markdown](https://www.markdownguide.org/getting-started/)