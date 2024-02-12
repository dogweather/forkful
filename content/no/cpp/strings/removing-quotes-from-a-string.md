---
title:                "Fjerne anførselstegn fra en streng"
aliases:
- /no/cpp/removing-quotes-from-a-string/
date:                  2024-01-26T03:38:16.471903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng betyr å skrelle bort disse plagsomme doble eller enkle tegnene som omslutter teksten vår (' eller "). Programmerere gjør ofte dette for å rense inndata, lagre tekst i en database, eller forberede strenger for videre behandling uten rotet av anførselstegn.

## Hvordan:
Her er en grei måte å sparke disse anførselstegnene til fortauskanten i C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hei, 'Verden'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Kjør dette, og du får:

```
Hei, Verden!
```

Voilà! Anførselstegnene har forsvunnet.

## Dypdykk
Anførselstegn har vært en tekstplage siden datateknologiens morgen. I gamle dager ville du se programmerere møysommelig loope gjennom hver karakter for å filtrere ut disse anførselstegnene. I dag har vi `std::remove` i Standard Template Library (STL) for å gjøre det tunge løftet.

Alternativer? Klart det! Du kunne bruke regulære uttrykk med `std::regex` for å målrette anførselstegn, men det er litt som å bruke en slegge til å knuse en nøtt - kraftfullt, men kan være overkill for enkle oppgaver. For de som foretrekker nyere C++ smaker, kan du eksperimentere med `std::string_view` for ikke-modifiserende tilnærminger.

Når det gjelder implementeringen, husk at `std::remove` faktisk ikke fjerner elementer fra beholderen; det omorganiserer de ikke-fjernede elementene fremover og returnerer en iterator forbi den nye slutten av rekkevidden. Derfor trenger vi `erase`-metoden for å kutte av den uønskede halen.

## Se også
- C++ `std::remove` referanse: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Mer om `std::string` manipulasjon: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
