---
title:                "C++: Å starte et nytt prosjekt"
programming_language: "C++"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt programmeringsprosjekt kan være en spennende og lærerik opplevelse. Det er en måte å utforske nye ideer og utvide dine ferdigheter innen programmering. I tillegg kan det være en nyttig måte å løse problemer eller forenkle arbeidsoppgaver på.

## Hvordan

For å starte et nytt prosjekt i C++, må du først sette opp miljøet ditt. Dette betyr å installere en C++ kompilator og en integrert utviklingsmiljø (IDE) som Visual Studio eller Code::Blocks. Dette vil tillate deg å skrive og kjøre C++ kode på datamaskinen din.

Neste steg er å bestemme hva prosjektet ditt skal gjøre og lage en plan. Dette kan være alt fra å løse et matematisk problem til å utvikle en applikasjon. Deretter kan du begynne å skrive koden din ved å følge disse trinnene:

1. Deklarer de nødvendige variablene og datatypene du trenger.
2. Bruk inputfunksjoner for å få brukerens inndata hvis det er nødvendig.
3. Bruk matematiske og logiske operatorer for å behandle dataene.
4. Bruk utskriftsfunksjoner for å vise resultatet til brukeren.

Når koden din er skrevet, kan du kjøre den og se om den oppfører seg som forventet. Hvis ikke, kan du feilsøke problemene ved å bruke debugging-funksjoner i IDE-en din.

### Eksempel:

```C++
#include <iostream>

using namespace std;

int main(){
    int a, b;
    cout << "Tast inn to tall: ";
    cin >> a >> b;
    int sum = a + b;
    cout << "Summen av " << a << " og " << b << " er " << sum << endl;
    return 0;
}
```

Dette kodeeksempelet ber brukeren om å skrive inn to tall og beregner deretter og viser summen av disse tallene.

## Dypdykk

Når du starter et nytt prosjekt, er det viktig å tenke på kodespråket og strukturen du vil bruke. I C++ må du ha en main() funksjon som er startpunktet for programmet ditt. Denne funksjonen må returnere en verdi, vanligvis `0`, for å indikere at programmet ble kjørt uten feil.

I tillegg er det viktig å organisere koden din ved å bruke funksjoner, kontrollstrukturer og kommentarer for å gjøre den mer lesbar og enklere å feilsøke. Planlegg også hvordan du vil håndtere feil og unntak som kan oppstå under kjøringen av programmet ditt.

Et annet aspekt å vurdere er å bruke kodeversjonskontrollsystemer som Git for å spore endringer og samarbeide med andre på prosjektet ditt.

## Se også

- [C++ dokumentasjon](https://www.cplusplus.com/)
- [Lær C++ på Codecademy](https://www.codecademy.com/learn/learn-c-plus-plus)
- [Begynnerveiledning for C++ utvikling med Visual Studio](https://docs.microsoft.com/en-us/cpp/get-started/tutorial-console-cpp?view=vs-2019)