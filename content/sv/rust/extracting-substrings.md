---
title:                "Rust: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera delsträngar är en vanlig programmeringsuppgift som ofta används för att manipulera strängar på ett effektivt sätt. I Rust, ett populärt programmeringsspråk som kännetecknas av sitt fokus på säkerhet och prestanda, finns det flera sätt att utföra denna uppgift. I denna bloggpost kommer vi att utforska några av dessa metoder och förklara hur man kan använda dem i praktiken.

## Hur man gör det

Att extrahera substrängar i Rust är relativt enkelt och kan göras på flera olika sätt. Det vanligaste sättet är att använda metoden `slice()` som finns tillgänglig för alla strängar. Detta gör att man kan ange en start- och en slutposition för den del av strängen som man vill extrahera.

```Rust
let s = "Hej alla Rust-programmerare";

let delstrang = &s[4..7]; // extraherar "alla" från strängen
```

Man kan också ange enbart startpositionen och låta slutpositionen vara öppen, vilket innebär att alla tecken från starten fram till slutet av strängen kommer att extraheras.

```Rust
let s = "Hej alla Rust-programmerare";

let delstrang = &s[8..]; // extraherar "Rust-programmerare" från strängen
```

Om man istället vill extrahera en delsträng baserat på ett specifikt tecken eller teckensekvens kan man använda metoden `split()` tillsammans med `collect()` för att skapa en vektor av delsträngar.

```Rust
let s = "Majs, kyckling, ris, grönsaker";

let delstranger: Vec<&str> = s.split(", ").collect(); // extraherar ["Majs", "kyckling", "ris", "grönsaker"]
```

Det finns också andra användbara metoder för att extrahera delsträngar, såsom `trim()` som tar bort whitespace från början och slutet av strängen, och `replace()` som ersätter en viss del av strängen med en annan.

## Djupdykning

När vi extraherar delsträngar i Rust skapas det egentligen en referens till en del av den ursprungliga strängen. Detta innebär att om vi ändrar den extraherade delen kommer det också att påverka den ursprungliga strängen. Detta är en viktig aspekt att tänka på när man arbetar med strängar i Rust.

En annan viktig detalj är att eftersom strängar är immutabla i Rust, innebär detta att vissa av metoderna för att extrahera delsträngar faktiskt returnerar en ny kopia av den ursprungliga strängen istället för att ändra den befintliga strängen. Detta bör också tas i beaktning när man väljer vilken metod man vill använda för att extrahera delsträngar.

## Se också

- [Rust dokumentation för strängar](https://doc.rust-lang.org/std/string/)
- [Rust By Example: Strängar](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rustlings: Strängar](https://rust-lang.github.io/rustlings/exercises/strings/README.html)