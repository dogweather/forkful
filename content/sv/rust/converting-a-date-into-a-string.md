---
title:                "Omvandla ett datum till en sträng"
html_title:           "Bash: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

---
title: "Datatypomvandling i Rust: Datum till Sträng"
---

## Vad & Varför?
Att omvandla ett datum till en sträng är processen att skapa en läsbar strängrepresentation av ett datum. Vi gör detta för att på ett mänskligt läsbart sätt visa datum och tid.

## Så här gör du:
I Rust kan du använda `chrono`-biblioteket för att enkelt konvertera ett `DateTime`-objekt till en sträng. Här är ett exempel:

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let nu: DateTime<Utc> = Utc::now();
    println!("{}", nu.to_string());
}
```

När du kör detta skrivs nuvarande datum och tid ut som en sträng, t.ex. "2022-03-01T13:43:22.133366Z".

## Fördjupning
Rust, sedan sitt utfärdande 2010, har alltid prioriterat tydlighet och säkerhet, vilket betyder att datatypomvandlingarna är enkla och säkra.

Som alternativ till `DateTime<Utc>::to_string()`, kan du använda `format!`-makro för mer anpassade datumsträngformat. Till exempel:

```Rust
let fmt = nu.format("%Y-%m-%d %H:%M:%S").to_string();
println!("{}", fmt);
```

Ovanstående kod skriver ut datum- och tidssträngen i formatet "2022-03-01 13:43:22".

För att genomföra denna konvertering behöver Rust fånga den aktuella tiden, översätta det till en intern representation, och använda sedan strängformateringsverktyget för att konvertera den interna representationen till en sträng som kan visas.

## Se också
För mer detaljerad information, kolla in följande resurser:

- Rusts officiella dokumentation för [`DateTime`](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html)
- `chrono`-bibliotekets [GitHub-sida](https://github.com/chronotope/chrono)
- Rusts officiella dokumentation för [`to_string`](https://doc.rust-lang.org/std/string/trait.ToString.html)
- Rusts officiella dokumentation för [`format!`](https://doc.rust-lang.org/std/macro.format.html)

Allt omfattat ovan borde ge dig en klar idé om hur du konverterar datumobjekt till strängar i ditt Rust-program. Lycka till!