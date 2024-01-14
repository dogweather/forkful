---
title:                "Rust: Utvinna substränger"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle någon vilja extrahera substrängar? Det finns många scenarier där det kan vara användbart, som att bearbeta text från en användare eller filtrera data från en fil. Med Rusts kraftfulla stränghanteringsfunktioner kan du enkelt extrahera önskade delar av en sträng för att använda i ditt program.

## Så här gör du
För att extrahera en substräng i Rust kan du använda metoden `.slice()`. Till exempel, om du har en sträng som heter "Hej, jag heter Svensk Rust" och du vill extrahera "Svensk Rust", kan du göra så här:

```rust
let string = "Hej, jag heter Svensk Rust";
let slice = string.slice(16..); // skär ut från index 16 till slutet av strängen
println!("{}", slice); // kommer att skriva ut "Svensk Rust"
```

Du kan också använda metoden `.chars()` för att få en slice av enskilda tecken i en sträng. Om du till exempel har en sträng som heter "abcde" och du bara vill ha de första tre bokstäverna, kan du göra det så här:

```rust
let string = "abcde";
let chars = string.chars().take(3); // tar de första tre bokstäverna
println!("{:?}", chars); // kommer att skriva ut ["a", "b", "c"]
```

Om du behöver extrahera en specifik del av en sträng baserat på ett visst mönster kan du använda regex med hjälp av paketet `regex` från Rusts standardbibliotek. Till exempel kan du extrahera alla siffror från en sträng genom att använda en regex som matchar siffror:

```rust
let string = "Detta är ett tal: 12345";
let re = Regex::new(r"\d+").unwrap(); // skapar en regex som matchar en eller flera siffror
let digits: String = re.find(string).unwrap().as_str().to_string(); // extraherar siffrorna (12345) och konverterar dem till en sträng
println!("{}", digits); // kommer att skriva ut "12345"
```

## Djupdykning
Att extrahera substrängar i Rust är snabbt och effektivt tack vare Rusts string slices, som inte kräver att en ny sträng skapas och allokeras i minnet. Detta leder till bättre prestanda och minskad risk för minnesläckor.

En annan fördel med att använda Rust för att extrahera substrängar är dess djupa integrering med mönstermatchning. Genom att använda match-syntaxen kan du enkelt extrahera specifika delar av en sträng baserat på mönster, vilket sparar tid och gör koden mer läsbar.

## Se även
- Rust Dokumentation: Strängar https://doc.rust-lang.org/std/string/
- Rust Dokumentation: Regex https://doc.rust-lang.org/regex/
- Officiell Rust hemsida https://www.rust-lang.org/