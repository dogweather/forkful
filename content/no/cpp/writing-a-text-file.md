---
title:    "C++: Skriver en tekstfil"
keywords: ["C++"]
---

{{< edit_this_page >}}

### Hvorfor
Det å skrive en tekstfil er en viktig del av programmering fordi det gir oss muligheten til å lagre data på en strukturert og varig måte. Dette kan være nyttig for å lagre informasjon som skal brukes igjen senere, eller for å organisere store mengder data.

### Slik Gjør Du Det
Det å skrive en tekstfil i C++ er ganske enkelt. Først må du inkludere "fstream" biblioteket. Deretter åpner du en fil ved å bruke "ofstream" funksjonen og oppgir navnet på filen du vil opprette. Deretter kan du begynne å skrive til filen ved å bruke << operatøren. Til slutt må du lukke filen ved å bruke close() funksjonen. Se et eksempel under:

```C++
#include <fstream>

int main(){
    // åpner en fil ved navn "mitt_file.txt" for skriving
    std::ofstream min_fil("mitt_file.txt");

    // skriver til filen ved hjelp av << operatøren
    min_fil << "Dette er en tekst som skal skrives til filen.\n";

    // lukker filen
    min_fil.close();

    return 0;
}
```
Etter at du har kjørt denne koden, vil du ha opprettet en tekstfil kalt "mitt_file.txt" som inneholder teksten "Dette er en tekst som skal skrives til filen.".

### Dykk Dypere
Når vi skriver en tekstfil, kan vi også bruke "ofstream" funksjonen i "appen" modus, som vil legge til ny informasjon til slutten av filen i stedet for å overskrive den. Dette kan være nyttig hvis du ønsker å legge til mer data til en eksisterende fil. For å gjøre dette, bruker du "ofstream::app" flagget når du åpner filen. Se et eksempel under hvor vi legger til flere linjer til slutten av "mitt_file.txt" filen:

```C++
#include <fstream>

int main(){
    // åpner filen i "appen" modus
    std::ofstream min_fil("mitt_file.txt", std::ofstream::app);

    // legger til flere linjer til filen
    min_fil << "Dette er de nye linjene som blir lagt til filen.\n";
    min_fil << "Dette er en annen linje som blir lagt til filen.\n";

    // lukker filen
    min_fil.close();

    return 0;
}
```

### Se Også
- [C++ fstream dokumentasjon](https://www.cplusplus.com/reference/fstream/)
- [Les og skriv til filer i C++](https://www.geeksforgeeks.org/read-write-file-c/)
- [Matrix-kodeblogg](https://www.matrix.no/blogg/)