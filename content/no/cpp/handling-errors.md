---
title:                "Feilhåndtering"
date:                  2024-01-26T00:49:58.215725-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Feilhåndtering betyr å planlegge for når ting går galt. Det er avgjørende fordi det hjelper med å unngå krasjer og gjør programvaren din robust og brukervennlig.

## Hvordan:
Her er en grunnleggende try-catch blokk for å håndtere et unntak:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Oops! Noe gikk galt.");
    } catch (const std::exception& e) {
        std::cerr << "Feil: " << e.what() << std::endl;
    }
    return 0;
}
```

Eksempel på utskrift:
```
Feil: Oops! Noe gikk galt.
```

## Dykk dypere
C++ har hatt feilhåndtering siden sine tidlige dager. Den mest grunnleggende formen var å sjekke returverdier. Hvis du har vært med en stund, husker du før-standard dagene: C med klasser og manuell feilsjekking.

Så kom unntak med C++ for å gi oss en strukturert måte å håndtere uventede problemer på. Et unntak blir kastet med `throw` og fanget med `try/catch`.

To typer feil dukker ofte opp: logiske feil, som en feil kalkulasjon, og kjøretidsfeil, som å få tilgang til en ugyldig minneadresse. Unntak er ideelt for kjøretidsfeil. For logiske feil er det ofte bedre å bruke påstander eller feilkoder.

Det pågår en kontinuerlig debatt om unntak vs. feilkoder. Unntak kan være tregere og kan føre til komplekse kontrollflyter. Feilkoder, mens de er raskere, kan gjøre kode rotete og vanskeligere å vedlikeholde. Det er en avveining, så å vite bruksområdet ditt er nøkkelen.

C++17 introduserte `std::optional` og `std::variant`, som er alternativer til unntak. De er nyttige for funksjoner som kanskje ikke returnerer et gyldig resultat.

Unntakssikkerhet kan være en annen hodepine. Det handler om garantier koden din gir til tross for unntak. Det er tre nivåer: grunnleggende, sterk og nothrow. Jo flere garantier, jo mer kompleks kan koden din være.

Avsluttende tanker - feilhåndtering er like mye kunst som vitenskap. Det former hvordan applikasjonen din overlever i det fri. Misbruk ikke unntak. Mål for lesbart, vedlikeholdbart kode.

## Se også
- [cppreference om håndtering av unntak](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrups syn på feilhåndtering](http://www.stroustrup.com/except.pdf)
- [C++ Core Guidelines om unntak](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)