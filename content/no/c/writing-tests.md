---
title:                "C: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor?

Mange utviklere vurderer ofte å skrive tester som en "unødvendig" eller "ekstra" oppgave. De tror kanskje at de kunne bruke den tiden og energien til å skrive ny kode i stedet. Men i virkeligheten kan det å skrive tester ha mange fordeler for koden din. Det kan hjelpe deg med å identifisere og fikse feil tidlig i utviklingsprosessen, forbedre kodekvaliteten og gjøre debugging enklere. Ved å skrive tester viser du også at du bryr deg om å levere pålitelig og feilfri kode til dine brukere.

# Hvordan?

Det er enkelt å komme i gang med å skrive tester i C-programmering. Her er et eksempel på hvordan du kan teste en enkel funksjon som legger sammen to tall og returnerer resultatet:

```C
#include <stdio.h>

// Funksjon for å legge sammen to tall
int add(int num1, int num2) {
    return num1 + num2;
}

// Testfunksjon for add() funksjonen
void test_add() {
    int result = add(5, 5);
    if (result == 10) {
        printf("Testen passerte!\n");
    } else {
        printf("Testen feilet. Forventet 10, fikk %d\n", result);
    }
}

int main() {
    // Kaller test_add() funksjonen
    test_add();

    return 0;
}
```

I dette eksempelet har vi en funksjon som legger sammen to tall, og en testfunksjon som kaller den og sjekker om resultatet er som forventet. Kompilering og kjøring av dette programmet vil gi følgende output:

```
Testen passerte!
```

Dette betyr at testen vår var vellykket og funksjonen vår fungerer som den skal.

# Dypdykk

Det er viktig å huske på at tester bør være en integrert del av utviklingsprosessen. Det betyr at du bør skrive tester mens du koder, ikke etter at koden er ferdig. Dette vil hjelpe deg med å finne og fikse feil mens de fortsatt er ferske og enklere å løse.

Det finnes også forskjellige typer tester som du kan skrive i C-programmering, som f.eks. enhetstester og integrasjonstester. Enhetstester tester individuelle funksjoner eller moduler, mens integrasjonstester tester hvordan disse funksjonene fungerer sammen. Det er viktig å finne en balanse og skrive tester for både små og store deler av koden din.

Husk også på at selv om du skriver tester, betyr ikke det at koden din vil være feilfri. Tester kan bare hjelpe deg med å identifisere feil, men det er fortsatt viktig å ha god kvalitetskontroll og et godt øye for feil i koden din.

# Se også

- [Hvordan skrive effektiv og pålitelig kode i C](https://linktilside.com)
- [10 tips for å forbedre din C-programmeringskunnskap](https://linktilside.com)
- [En enkel veiledning til enhetstesting i C](https://linktilside.com)