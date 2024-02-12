---
title:                "Organisering av kode i funksjoner"
aliases:
- /no/rust/organizing-code-into-functions.md
date:                  2024-01-26T01:16:12.090645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner handler om å bryte programmet ditt ned i gjenbrukbare, modulære biter identifisert ved et navn. Vi gjør dette for å gjøre koden vår renere, mer lesbar og enklere å feilsøke. Det handler om å ikke gjenta oss selv og å effektivisere oppdateringer.

## Hvordan:
Si at du har kode som regner ut arealet av en sirkel flere ganger. I stedet for å gjenta formelen, pakker du den inn i en funksjon.

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("Arealet av sirkelen er: {}", area);
}
```

Output:

```
Arealet av sirkelen er: 78.53981633974483
```

## Dypdykk
Historisk kommer funksjoner fra matematikk, der de mapper inndata til utdata. I programmering har de vært rundt siden dager med samlekoder, selv om vi kalte dem 'subrutiner'. Rust-funksjoner kan returnere verdier og til og med andre funksjoner takket være førsteklasses funksjoner og lukninger.

Alternativer? Innebygd kode eller makroer, men de er rotete for kompleks logikk. Objekter med metoder er en annen måte å organisere funksjonalitet på, en annen smak enn frittstående funksjoner.

Implementasjon i Rust er ganske grei. Funksjoner erklærer deres parametertyper og returtype. De bruker 'snake case' for navngiving etter konvensjon. Du har dine offentlige funksjoner (`pub fn`) for bruk utenfor modulen og private for internt bruk. Og Rust har denne kule funksjonen hvor du ikke trenger et `return`-nøkkelord for det siste uttrykket i en funksjon.

## Se Også
Sjekk ut disse for mer info:
- Boken "The Rust Programming Language": [Funksjoner](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust ved Eksempel om [Funksjoner](https://doc.rust-lang.org/rust-by-example/fn.html)
