---
title:                "Rust: Utskrift av feilsøkingsutdata"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe ut feilsøkingsutskrift er en viktig del av programmering. Det lar deg se hva som skjer i koden din og finne ut av eventuelle feil eller problemer som oppstår. Dette kan spare deg for mye tid og frustrasjon når du jobber med større og mer komplekse prosjekter.

## Hvordan

Det er ganske enkelt å printe ut feilsøkingsutskrift i Rust ved hjelp av makroen "println!". Denne makroen tar imot en streng som argument og skriver det ut på skjermen. Her er et eksempel på hvordan du kan bruke den i koden din:

```Rust
let navn = "Per";
let alder = 30;

println!("Navnet mitt er {} og jeg er {} år gammel", navn, alder);
```

Dette vil skrive ut følgende:

```
Navnet mitt er Per og jeg er 30 år gammel
```

Du kan også bruke "format!"-makroen for å formatere output på en mer spesifikk måte. Denne makroen fungerer på samme måte som "println!"-makroen, med unntak av at den ikke skriver ut noe på skjermen. I stedet returnerer den en formatert streng som du kan skrive ut ved hjelp av "println!"-makroen. Her er et eksempel på hvordan du kan bruke den:

```Rust
let tall = 10;
let kvadrat = format!("{} * {} = {}", tall, tall, tall * tall);

println!("{}", kvadrat);
```

Dette vil skrive ut følgende:

```
10 * 10 = 100
```

## Dypdykk

Mens "println!"-makroen er nyttig for å raskt å printe ut feilsøkingsutskrift, er det også noen andre måter å gjøre det på i Rust. En av disse er "dbg!"-makroen, som både skriver ut verdien og returnerer den. Dette er spesielt nyttig for å sjekke verdien av en variabel midt i en kjede av metoder eller funksjoner. Her er et eksempel på hvordan du kan bruke den:

```Rust
let tall = 10;
let dobbelt = tall * 2;
let kvadrat = dbg!(dobbelt * 2);

println!("{}", kvadrat);
```

Dette vil skrive ut følgende:

```
20
```

I tillegg til å bruke makroer, kan du også bruke "eprintln!"-makroen for å skrive til standard feilstrøm, i stedet for standard utstrøm. Dette kan være nyttig når du vil skrive ut feilmeldinger eller andre typer output som skal skille seg fra vanlig output. Du kan også bruke "dbg!(""-makroen for å skrive feilsøkingsutskrift direkte til standard feilstrøm.

## Se også

- [Offisiell Rust dokumentasjon for feilsøkingsutskrift](https://doc.rust-lang.org/std/macro.println.html)
- [Rust By Example - Printing output](https://doc.rust-lang.org/rust-by-example/hello/print.html)
- [Rust Book - Printing output](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html#printing-values-with-println-and-string-interpolation)