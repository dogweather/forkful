---
title:    "Rust: Utvinna delsträngar"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att extrahera delsträngar är en viktig del av många programmeringsuppgifter, särskilt när man arbetar med text eller datasträngar. I Rust finns det flera inbyggda funktioner som kan hjälpa till med detta, vilket gör det till ett kraftfullt verktyg för hantering av delsträngar.

## Hur man gör det

För att extrahera en delsträng i Rust kan man använda funktionen `substring()` som finns inbyggd i standardbiblioteket. För att använda denna funktion behöver man först skapa en variabel som innehåller den ursprungliga strängen och sedan ange vilket område av strängen man vill extrahera. Till exempel:

```Rust 
let sträng = "Hej alla!";
let delsträng = sträng.substring(4..8);
println!("{}", delsträng); 
```

Detta kommer att skriva ut `alla` på skärmen, eftersom det är tecken 4 till 8 i strängen `Hej alla!`. I detta exempel används ett index som börjar på 0, så `4..8` motsvarar egentligen tecken 5 till 8 i strängen.

## Djupdykning

I Rust är det också möjligt att extrahera delsträngar med hjälp av `chars()`-funktionen. Detta är särskilt användbart när man vill extrahera ett visst tecken eller en viss del av en sträng baserat på ett visst villkor. Till exempel kan man använda `chars()`-funktionen för att extrahera alla versaler från en sträng, eller alla siffror. Det finns också andra avancerade funktioner som `split()` och `replace()` som kan vara användbara för att extrahera delsträngar i mer komplicerade scenarier.

## Se också

- [Dokumentation för substring()](https://doc.rust-lang.org/std/str/fn.substring.html)
- [Guide till strängar i Rust](https://www.rust-lang.org/learn/strings)
- [Mer om hantering av delsträngar i Rust](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html)