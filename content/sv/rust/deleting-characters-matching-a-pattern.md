---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Rust: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi programmerar i Rust kan vi ofta stöta på situationer där vi behöver ta bort tecken som matchar ett visst mönster. Detta kan göras med hjälp av inbyggda funktioner i Rust som gör det möjligt att filtrera och manipulera textsträngar.

Att ta bort tecken som matchar ett mönster är ett vanligt förekommande problem i programmering, särskilt när vi arbetar med data eller hanterar användarinput. Det kan hjälpa oss att rensa oönskade tecken eller extrahera specifika bitar av information från en textsträng.

## Hur gör man:
Enklaste sättet att ta bort tecken som matchar ett mönster i Rust är genom att använda funktionen `replace`, som kan hittas i standardbiblioteket `std::string::String`. Denna funktion tar två argument: ett mönster att matcha och en ersättningssträng. Alla förekomster av mönstret i strängen kommer att ersättas med ersättningssträngen.

```Rust
let text = String::from("Hej, välkommen!");
let filtered_text = text.replace("e", ""); // hittar alla 'e' och tar bort dem
println!("{}", filtered_text); // Hj, välkommn!
```

Vi kan också använda funktionen `rm` från `regex` biblioteket för att ta bort tecken som matchar ett mer komplicerat mönster, till exempel alla siffror från en textsträng.

```Rust
extern crate regex;
use regex::Regex;

let text = "Ring mig på 070-1234567!";
let re = Regex::new(r"[0-9]+").unwrap();
let filtered_text = re.replace_all(text, ""); // tar bort alla siffror
println!("{}", filtered_text); // Ring mig på -!
```

## Djupdykning:
Att ta bort tecken som matchar ett mönster är en viktig del av textmanipulering och stränghantering i programmering. Detta koncept har funnits länge och de flesta moderna programmeringsspråk har inbyggda funktioner för att hantera det.

En alternativ metod för att ta bort tecken som matchar ett mönster är att använda funktionen `trim`, som tar bort alla förekomster av ett visst tecken (vanligtvis mellanslag) från början och slutet av en sträng.

```Rust
let text = "   Hej och välkommen!   ";
let trimmed_text = text.trim(); // tar bort alla mellanslag från början och slutet
println!("{}", trimmed_text); // Hej och välkommen!
```

Implementationen av `replace` och `rm` funktionerna är baserade på reguljära uttryck, vilket är en kraftfull metod för strängmatchning. Dessa uttryck kan användas för att hitta specifika mönster inom en textsträng och utföra olika operationer på dem.

## Se även:
- [Rust Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Regex Biblioteket](https://github.com/rust-lang-nursery/regex)