---
title:    "Rust: Konvertere en streng til små bokstaver"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig for å standardisere data eller utføre sammenligninger mellom tekster. Dette gjøres ofte i programmering for å sikre at inputen er konsistent og forventet.

## Slik gjør du det

Det er enkelt å konvertere en streng til små bokstaver i Rust ved hjelp av `to_lowercase()`-funksjonen. Her er et eksempel på hvordan man gjør det:

```Rust
let tekst = "Hei, dette er en TEST!";
let konvertert_tekst = tekst.to_lowercase();

println!("{}", konvertert_tekst);
```

Output:

```bash
hei, dette er en test!
```

Vi kan også konvertere kun en del av strengen ved å bruke `to_lowercase()`-funksjonen på en spesifikk del av strengen. For eksempel:

```Rust
let tekst = "Hei, dette er en TEST!";
let første_del = &tekst[0..3]; // henter første tre bokstavene i strengen
let andre_del = &tekst[4..]; // henter resten av strengen

let endret_første_del = første_del.to_lowercase();

let result = format!("{}{}", endret_første_del, andre_del);

println!("{}", result);
```

Output:

```bash
hei, dette er en TEST!
```

## Dykk dypere

I Rust er alle tegn i en streng representert ved hjelp av UTF-8-encoding. Dette betyr at å konvertere en streng til små bokstaver ikke bare innebærer å endre det engelske alfabetet fra store til små bokstaver, men også å håndtere andre språk og tegnsett. 

Det er også viktig å tenke på at enkelte Unicode-tegn kan bestå av flere tegn, og derfor vil ikke alle Unicode-tegn bli konvertert til små bokstaver ved bruk av `to_lowercase()`-funksjonen alene. Det er derfor viktig å undersøke og forstå hvordan denne funksjonen fungerer for å sikre korrekt konvertering.

## Se også

- [Rust dokumentasjon for `to_lowercase()`](https://doc.rust-lang.org/std/string/trait.ToLowercase.html)
- [GitHub-eksempel på konvertering av streng til små bokstaver i Rust](https://github.com/tomprogrammer/string_case_converter)