---
title:    "C++: Skriver til standardfeil"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Hvorfor

Å skrive til standard error (stderr) er en viktig del av feilretting og feilhåndtering i C++. Når et program kjører, kan det oppstå feil som må håndteres, og ved å skrive til stderr kan utviklere få mer innsikt i hva som gikk galt og hvorfor.

# Hvordan

For å skrive til stderr i C++, bruker vi funksjonen `std::cerr`. Dette er en del av standardbiblioteket i C++, så vi trenger ikke å importere noe ekstra for å bruke den. La oss se på et enkelt eksempel:

```C++ 
#include <iostream>

int main() {
  int x = 10;
  int y = 0;

  // Skriver til standard error hvis y er lik 0
  if (y == 0) {
    std::cerr << "Feil: Kan ikke dividere med 0" << std::endl;
    return 1;
  }

  // Hvis ingen feil oppstod, skriver vi resultatet til standard output
  std::cout << "Resultatet er: " << x / y << std::endl;
  return 0;
}
```

Her sjekker vi om `y` er lik 0, og hvis det er tilfellet, skriver vi en feilmelding til stderr ved hjelp av `std::cerr`. Deretter returnerer vi en ikke-null verdi for å indikere at programmet avsluttet med en feil. Hvis ingen feil oppstod, skriver vi resultatet til standard output ved hjelp av `std::cout`.

Når vi kjører dette programmet, vil vi få følgende output:

```
Feil: Kan ikke dividere med 0
```

Dette viser at meldingen ble skrevet til stderr og ikke forstyrret standard output som viser resultatet vårt.

# Dypdykk

Det er viktig å merke seg at `std::cerr` er en strøm (stream), akkurat som `std::cout`. Dette betyr at vi kan bruke alle de samme formateringsfunksjonene som vi bruker med `std::cout`, som for eksempel `<<` operator og `endl`.

En annen viktig funksjon av `std::cerr` er at den ikke er buffret, noe som betyr at meldingene vi skriver til stderr vil bli skrevet umiddelbart uten å vente på at buffere skal tømmes. Dette er spesielt nyttig i situasjoner der programmet vårt krasjer og vi trenger å vite hva som var siste melding som ble skrevet før programmet stoppet.

# Se også

* [C++ Standard Library: iostream](https://en.cppreference.com/w/cpp/header/iostream)
* [C++ Standard Library: cerr](https://en.cppreference.com/w/cpp/io/cerr)