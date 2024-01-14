---
title:    "Rust: Sökning och ersättning av text"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är ett vanligt problem vid programmering, och därför är det viktigt att ha en effektiv lösning för detta. Med Rusts inbyggda funktioner och kraftfulla strängbehandling kan du enkelt och pålitligt söka och ersätta text i dina kodfiler.

## Så gör du

För att söka och ersätta text i en sträng i Rust, kan du använda `replace()` funktionen. Här är ett exempel på hur du kan använda den:

```rust
let str = "Hej, jag heter Rust!";
let ny_str = str.replace("Rust", "swärje");

println!("{}", ny_str); // Utskrift: Hej, jag heter swärje!
```

Låt oss bryta ner detta kodexempel för att förstå vad som händer. Först skapar vi en variabel `str` som innehåller en sträng, sedan använder vi `replace()` funktionen för att byta ut ordet "Rust" mot "swärje". Slutligen skriver vi ut den nya strängen med `println!()` funktionen.

Det är viktigt att notera att `replace()` funktionen returnerar en ny sträng, så om du vill behålla den ursprungliga strängen måste du tilldela resultatet till en ny variabel, som i exemplet ovan.

## Djupdykning

Som standard söker `replace()` funktionen endast efter en matchning av det första förekommande mönstret. Men det finns också möjlighet att söka och ersätta alla förekommande mönster i strängen. Detta kan göras genom att använda en nyckelordet `replace_all()` istället för `replace()`.

```rust
let str = "Ha en bra dag!";
let ny_str = str.replace_all("a", "å");

println!("{}", ny_str); // Utskrift: Hå en brå dag!
```

Här ser vi att vi bytt ut alla förekomster av bokstaven "a" mot bokstaven "å" i den ursprungliga strängen.

Det finns också möjlighet att göra avancerade sökningar med hjälp av reguljära uttryck genom att använda `regex_replace()` funktionen från regex biblioteket. Detta ger en ännu mer kraftfull lösning för att söka och ersätta text i dina kodfiler.

## Se även

- [Rust dokumentation om strängar](https://doc.rust-lang.org/std/string/index.html)
- [Regex biblioteket dokumentation](https://docs.rs/regex/latest/regex/)
- [En guide till reguljära uttryck i Rust](https://rust-lang-nursery.github.io/regex/book/)

Nu är du väl rustad för att söka och ersätta text i dina Rust-program! Lycka till med programmeringen! 🚀