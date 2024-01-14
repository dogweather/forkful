---
title:                "Rust: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Concatenating, eller att sammanfoga, strängar är en vanlig uppgift vid programmering. Det är en metod för att sammanfoga flera strängar, eller ord, till en enda sträng. Detta kan vara användbart för att skapa dynamisk text, som till exempel för användargränssnitt eller loggar. På grund av dess vanliga förekomst är det en viktig färdighet för programmerare att ha.

## Hur man gör det

I Rust finns det flera sätt att utföra string concatenation, men det mest vanliga är att använda "format!" makron. Det här är en inbyggd makro som tar emot en formatteringssträng och en lista med värden, och returnerar en ny sträng som är sammansatt av dessa värden. Här är ett exempel på hur man skulle använda det:

```Rust
let first_name = "Erika";
let last_name = "Larsson";
let full_name = format!("{} {}", first_name, last_name);
println!("Välkommen, {}!", full_name);
```

Det här skulle skapa en ny sträng som heter "Erika Larsson" och sedan skriva ut "Välkommen, Erika Larsson!" i terminalen. I detta exempel används "{}" i formatteringssträngen för att indikera var värdena ska sättas in.

## Djupdykning

I Rust finns det också andra metoder för string concatenation, såsom "push_str" och "push". Dessa är dock mindre effektiva eftersom de kräver att programmeraren måste skapa en ny sträng och sedan ändra den, vilket kan bli resurskrävande. "Format!" makron är därför en mer föredragen metod.

En annan viktig aspekt att tänka på är att Rust har immutabla strängar som standard. Detta innebär att strängar inte kan ändras efter att de har skapats. Så när man sammanfogar strängar i Rust, måste man skapa en helt ny sträng istället för att bara lägga till på den befintliga, som man kanske skulle kunna göra i andra språk.

## Se även

- [Standard Library Documentation](https://doc.rust-lang.org/std/index.html)
- [The Rust Book: Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)