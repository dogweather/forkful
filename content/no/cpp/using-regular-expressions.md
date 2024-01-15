---
title:                "Å bruke regulære uttrykk"
html_title:           "C++: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor 

Hvis du jobber med tekstbehandling eller analyserer store mengder data, kan det være nyttig å kunne finne og manipulere tekstbaserte mønstre. Her kommer regulære uttrykk (regex) inn i bildet- kraftige verktøy som lar deg søke etter og behandle tekst på en effektiv måte.

## Hvordan du gjør det

Bruken av regulære uttrykk i C++ er enkelt, men krever en grundig forståelse av syntaksen. La oss ta en titt på noen grunnleggende eksempler:

 ```C++
 // Søk etter en streng i en tekst
 std::string tekst = "Dette er en tekst";
 std::regex reg ("tekst");
 std::smatch treff;
 if (std::regex_search(tekst, treff, reg)) {
    std::cout << "Vi fant '" << treff.str() << "' i teksten!" << std::endl;
 }
 // Output: Vi fant 'tekst' i teksten! 
 ```
 
 I dette eksemplet bruker vi `std::regex` for å lage et regulært uttrykk som søker etter strengen "tekst" i variabelen `tekst`. Når vi bruker funksjonen `std::regex_search`, lagrer vi treffet i variabelen `treff` og kan deretter bruke `treff.str()` for å få tilgang til selve strengen. 
 
 ```C++
 // Bytt ut et mønster med en annen streng
 std::string tekst = "Dette er en tekst";
 std::regex reg ("tekst");
 std::cout << std::regex_replace(tekst, reg, "avsnitt") << std::endl;
 // Output: Dette er en avsnitt 
 ```
 
 Her bruker vi `std::regex_replace` for å bytte ut strengen "tekst" med "avsnitt" i variabelen `tekst`. Den nye strengen blir så skrevet ut på skjermen. 
 
 Disse eksemplene er bare en liten del av hva som er mulig med regulære uttrykk. Søk og bytte ut mønstre, splitting av tekst og validering av input er alle mulige bruksområder for regex i C++.

## Dypdykk 

Å lage effektive og komplekse regulære uttrykk kan være en utfordring. Det finnes mange ressurser som kan hjelpe deg på veien, inkludert bøker og online regex-testere. Det er også viktig å forstå begrensningene til regulære uttrykk, spesielt når det kommer til mer avanserte oppgaver som å håndtere balanserte parenteser og nestede uttrykk. Det kan være lurt å kombinere regex med andre verktøy som for eksempel stringstreams og regulære uttrykk-flagg for å oppnå mer komplekse operasjoner.

## Se også

- [C++ Regex Library](https://en.cppreference.com/w/cpp/regex)
- [The Regular Expression Cookbook](https://www.oreilly.com/library/view/regular-expressions-cookbook/9780596802837/)
- [Regex101: Online Regex Tester](https://regex101.com/)