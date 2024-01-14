---
title:    "C++: Sletting av tegn som matcher et mønster."
keywords: ["C++"]
---

{{< edit_this_page >}}

# Hvorfor: Sletting av tegn som matcher et mønster

Noen ganger kan det være nødvendig å slette bestemte tegn som oppfyller et visst mønster i en tekst. Dette kan være for å filtrere ut uønsket informasjon eller for å gjøre tekstbehandling mer effektiv. Uansett grunn, så kan dette være en nyttig teknikk å lære for de som jobber med C++ programmering.

# Hvordan: Eksempler på koding og utdata

For å slette tegn som matcher et mønster, kan vi bruke en funksjon som heter `erase` fra den innebygde C++ strengklassen. Denne funksjonen tar inn to parametere - startindeksen og antall tegn som skal slettes. Ved å kombinere denne funksjonen med løkker og betingelser, kan vi enkelt lage en algoritme som sletter tegn basert på et gitt mønster.

```C++
#include <iostream>
#include <string>
using namespace std;

int main(){
    string tekst = "Hei alle sammen!";
    int index = 0, antall = 0;
    for (int i = 0; i < tekst.length(); i++) {
        // Sjekker om tegnet er et mellomrom
        if (tekst[i] == ' ') {
            antall++;
            // Sletter mellomrommet og tegnet etter
            // som oppfyller mønsteret
            tekst.erase(index, 2);
        } else {
            index++;
        }
    }
    cout << tekst << endl;
    return 0;
}
```

I dette eksempelet, sletter vi alle mellomrom og tegnene som kommer rett etter i en tekst, og ender opp med utdataen `Heiallesammen!`. Dette er bare et enkelt eksempel på hvordan vi kan bruke sletting av tegn for å endre en tekst etter et gitt mønster. Mulighetene er uendelige og avhenger av hva som er behovet i koden.

# Dypdykk: Mer informasjon om sletting av tegn som matcher et mønster

En ting å være oppmerksom på når man sletter tegn basert på et mønster, er at indeksene til tegnene i teksten vil endre seg etter hvert som tegn blir slettet. Dette betyr at hvis man for eksempel sletter tegnet på indeks 2, vil tegnet som var på indeks 3 nå ha indeks 2. Dette kan føre til feil i koden hvis man ikke tar hensyn til det.

En annen ting å huske på er at sletting av tegn kan være en uønsket måte å endre en tekst på hvis man ikke er forsiktig. Hvis man for eksempel sletter alle bokstavene som er lik "a", så ender man opp med et tekst uten bokstaven "a". Dette kan føre til at teksten blir uforståelig eller at informasjon går tapt. Derfor er det viktig å være klar over hva man gjør når man sletter tegn basert på et mønster.

# Se også

- [C++ string erase function](https://www.geeksforgeeks.org/stdstringerase-in-cpp/)
- [Tutorialspoint C++ string manipulation](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)