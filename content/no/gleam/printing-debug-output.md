---
title:    "Gleam: Utskrift av feilsøkingsoutput"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Hvorfor
Å skrive ut debug-utdata i Gleam kan være veldig nyttig når du prøver å finne feil i koden din. Det lar deg se verdier og ressurser i sanntid, noe som gjør feilsøking enklere og raskere.

# Hvordan 
For å skrive ut debug-utdata i Gleam, bruker du `gleam.io.debug` funksjonen. Denne funksjonen tar inn en verdi eller ressurs som argument, og skriver ut en forståelig representasjon av den. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
fn main() {
  let number = 42
  let name = "Gleam"

  // Skriver ut verdien av `number`
  gleam.io.debug(number)

  // Skriver ut verdien av `name`
  gleam.io.debug(name)
}

// Output:
// 42
// "Gleam"
```

Du kan også skrive ut flere verdier ved å separate dem med komma, som vist i eksemplet nedenfor:

```Gleam
fn main() {
  let first_name = "John"
  let last_name = "Doe"

  // Skriver ut både `first_name` og `last_name`
  gleam.io.debug(first_name, last_name)
}

// Output:
// "John", "Doe"
```

Dette kan være svært nyttig når du jobber med funksjoner som tar inn flere argumenter, og du vil vite hva som passerer gjennom dem.

# Deep Dive
Når du bruker debug-utdata, blir verdien eller ressursen konvertert til en streng som blir skrevet ut. Dette kan føre til at noen komplekse strukturer, som lister og tupler, ikke blir skrevet ut på en forståelig måte. For å løse dette kan du bruke `gleam.io.show` funksjonen. Denne funksjonen tar inn en vilkårlig verdi og returnerer en debug-streng som kan skrives ut med `gleam.io.debug` funksjonen. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
import gleam.list

fn main() {
  let names = ["John", "Jane", "Bob"]

  // Skriver ut verdiene i listen
  gleam.io.debug(names)

  // Skriver ut en forståelig representasjon av listen ved å bruke `gleam.io.show`
  gleam.io.debug(gleam.io.show(names))
}

// Output:
// [ "John", "Jane", "Bob" ]
// ["John", "Jane", "Bob"]
```

Det er også verdt å merke seg at `gleam.io.debug` og `gleam.io.show` bare fungerer under utvikling og vil ikke bli kompilert i produksjonskode. Derfor trenger du ikke bekymre deg for å fjerne disse kallene før du publiserer koden din.

# Se også
- [Gleam dokumentasjon: Debug Output](https://gleam.run/book/tutorials/debug-output.html)
- [Gleam API reference: gleam.io](https://gleam.run/modules/gleam_io.html)