---
title:                "Att göra en sträng storstavlad"
html_title:           "Rust: Att göra en sträng storstavlad"
simple_title:         "Att göra en sträng storstavlad"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Förmodligen har du stött på texter där alla första bokstäver är stora, till exempel rubriker eller namn. Det kallas för "kapitalisering", och det finns flera olika anledningar till varför man skulle vilja göra det.

Att kapitalisera en sträng kan ge texten en mer professionell och formell känsla, det kan underlätta läsningen och det kan vara en del av ett designmässigt koncept. Oavsett anledning, så är det en enkel uppgift att göra med Rust.

## Hur man gör

För att kapitalisera en sträng i Rust behöver du först importera en modul som innehåller funktionen `to_uppercase`. Detta kan göras genom att lägga till `use std::string::String;` längst upp i din kodfil.

Sedan kan du använda funktionen med en punktnotation på vilken sträng som helst, som till exempel `String::from("hello").to_uppercase()`.

Ett enklare sätt att göra detta på är med hjälp av en liten hjälpfunktion som tar emot en sträng och returnerar den kapitaliserade versionen, som följande:

```Rust
fn capitalize(text: &str) -> String {
    return String::from(text).to_uppercase();
}
```

När du använder funktionen så här: `let my_string = capitalize("hello");` kommer `my_string` att innehålla `HELLO`.

## Deep Dive

För de nyfikna finns det en del tekniskt bakom hur `to_uppercase` faktiskt fungerar. I grunden använder den en Unicode-tabell som innehåller små och stora bokstäver, och den jämför varje tecken med referenserna i tabellen för att avgöra om det ska bli stort eller litet.

Det finns också en variant av funktionen, `to_lowercase`, som gör motsatsen. För att behålla enbart den första bokstaven som stor kan du använda metoden `into_ascii_uppercase` eller `into_ascii_lowercase` i stället.

## Se även

Det finns många användbara funktioner som är inbyggda i Rust för att manipulera strängar på ett effektivt sätt. Här är några ytterligare resurser som du kan utforska för att lära dig mer:

- [String - Rust Standard Library](https://doc.rust-lang.org/std/string/struct.String.html)
- [Unicode - The Unicode Consortium](https://unicode.org/)
- [Ascii - The American Standard Code for Information Interchange](https://www.ascii-code.com/)