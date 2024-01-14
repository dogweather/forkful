---
title:                "Rust: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart när man vill rensa en textsträng eller söka efter specifika tecken. Det kan också vara en del av en större algoritm för att filtrera eller manipulera data.

## Så här gör du

Det finns flera sätt att ta bort tecken som matchar ett mönster i Rust. Ett sätt är att använda sig av metoden `.replace()` tillsammans med det typen `&str`, som representerar en textsträng. Här är ett grundläggande exempel:

```Rust
let text = "Jag älskar att läsa böcker.";
let ny_text = text.replace("älskar", "hatar");
println!("Den nya texten: {}", ny_text);
```

Output:
```
Den nya texten: Jag hatar att läsa böcker.
```

Om man istället vill ta bort alla mellanslag i en textsträng kan man använda en `for`-loop tillsammans med `.replace()`:

```Rust
let text = "Jag gillar att koda i Rust.";
let mut ny_text = String::new();
for char in text.chars() {
    if char != ' ' {
        ny_text.push(char);
    }
}
println!("Den nya texten: {}", ny_text);
```

Output:
```
Den nya texten: JaggillaratkodaiRust.
```

Det finns också en funktion som heter `.trim_matches()` som kan användas för att ta bort tecken från början eller slutet av en textsträng. I följande exempel tas alla vokaler bort från början av texten:

```Rust
let text = "irriterande";
let ny_text = text.trim_matches(['a', 'e', 'i', 'o', 'u'].as_ref());
println!("Den nya texten: {}", ny_text);
```

Output:
```
Den nya texten: rriterande
```

## Djupdykning

Att ta bort tecken som matchar ett mönster kan vara mer komplicerat än bara dessa exempel. Det kan innebära att använda sig av reguljära uttryck eller att implementera en egen algoritm för att hantera specialfall. Det är också viktigt att tänka på prestanda när man ska ta bort tecken från en stor textsträng, eftersom det kan påverka hastigheten i programmet.

## Se även

- [Rust Referens](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Reguljära uttryck i Rust](https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html)
- [Hantera textsträngar i Rust](https://www.educative.io/edpresso/what-is-the-use-of-the-string-in-rust)