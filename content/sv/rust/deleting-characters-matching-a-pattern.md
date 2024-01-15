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

## Varför

Ibland behöver vi ta bort specifika tecken från en sträng eller en fil för att uppnå ett visst mål. Det kan vara för att filtrera ut oönskade tecken eller för att ändra format på data. I det här avsnittet kommer vi att lära oss hur man tar bort tecken som matchar ett visst mönster i Rust.

## Hur man gör

För att ta bort tecken som matchar ett visst mönster i Rust, använder vi funktionen `replace_all` från standardbiblioteket `regex`. Vi börjar med att importera `regex` biblioteket genom att lägga till följande kod i början av vårt program:

```
use regex::Regex;
```

Nästa steg är att skapa en instans av `Regex` som innehåller mönstret som vi vill matcha. Det kan se ut så här:

```
let re = Regex::new(r"[aeiou]").unwrap();
```

Detta mönster kommer att matcha alla små bokstäver a, e, i, o, u i en sträng. Nu kan vi använda funktionen `replace_all` för att ersätta alla matchande tecken med en tom sträng. Detta kan göras så här:

```
let new_string = re.replace_all("Hello World!", "");
```

Detta kommer att ge oss en ny sträng utan några a, e, i, o, u tecken kvar.

## Djupdykning

Vid användning av `replace_all` funktionen är det viktigt att notera att den returnerar en helt ny sträng och lämnar den ursprungliga strängen oförändrad. Om vi vill ändra den ursprungliga strängen, måste vi tilldela det nya värdet till den ursprungliga variabeln. Till exempel:

```
let mut my_string = "This is a string".to_string();
let new_string = my_string.replace_all("is", "was");
```

Här kommer `new_string` att innehålla "Thwas was a string" medan `my_string` fortfarande är "This is a string".

Det är också värt att notera att `replace_all` funktionen är fallkänslig. Detta betyder att den kommer att ersätta tecken som matchar mönstret oavsett om de är stora eller små bokstäver. I exemplet ovan skulle "Hello World!" bli "Hll Wld!" eftersom vi inte specificerade att vi bara ville matcha små bokstäver.

## Se även

För mer information och exempel på hur man använder `replace_all` funktionen och `regex` biblioteket i Rust, se följande resurser:

- [Official Rust documentation](https://doc.rust-lang.org/std/str/struct.Regex.html)
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/science/mathematics/regex.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/std/str/regex.html)