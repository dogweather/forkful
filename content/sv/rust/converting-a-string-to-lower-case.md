---
title:    "Rust: Omvandla en sträng till små bokstäver"
keywords: ["Rust"]
---

{{< edit_this_page >}}

Rust: Konvertera en sträng till gemener

## Varför

Att kunna konvertera en sträng till gemener är en viktig färdighet för en programmerare, särskilt inom textbearbetning och användarinteraktion. Det kan användas för att jämföra strängar utan att behöva oroa sig för skillnader i stor eller liten bokstav, eller för att anpassa input från användaren till ett visst format.

## Så här gör du

För att konvertera en sträng till gemener i Rust, använd funktionen `to_lowercase()` från standardbiblioteket `std::string::String`. Här är ett enkelt exempel:

```Rust
fn main() {
    let input = String::from("Rust Programmering");
    let output = input.to_lowercase();

    println!("{}", output); // utmatning: rust programmering
}
```

I det här exemplet skapas en ny sträng `output` som är en kopia av `input`, fast med alla bokstäver omvandlade till gemener.

## Djupdykning

En djupare titt på `to_lowercase()` visar att den faktiskt använder en metod som kallas `make_ascii_lowercase()`, som är den som utför den faktiska konverteringen. Denna metod är en del av standardbiblioteket `std::ascii` och är beroende av koderna i ASCII-tabellen för att bestämma om en bokstav ska konverteras eller inte.

Detta innebär att konverteringen endast fungerar korrekt för ASCII-karaktärer, vilket kan leda till oönskade resultat om man försöker konvertera en sträng med specialtecken eller bokstäver utanför ASCII-tabellen. I sådana fall kan det vara mer lämpligt att istället använda `UnicodeNFD::to_lowercase()` från standardbiblioteket `std::unicode`, vilket gör en mer utförlig konvertering baserad på Unicode-standarder.

## Se även

- [Rust standardbiblioteket](https://doc.rust-lang.org/std/index.html)
- [ASCII-tabellen](https://www.ascii-code.com/)
- [Unicode standardbiblioteket](https://doc.rust-lang.org/std/unicode/)