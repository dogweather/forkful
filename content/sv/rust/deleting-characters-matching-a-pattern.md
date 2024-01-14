---
title:    "Rust: Radera tecken som matchar ett mönster"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför
När man programmerar i Rust, och speciellt när man arbetar med strängar, kan det ibland finnas behov av att ta bort vissa tecken som matchar ett visst mönster. Det kan till exempel vara för att konvertera en textsträng till ett nummer, eller för att rensa bort oönskade tecken från en inmatad sträng. Då kan det vara användbart att kunna radera tecken baserat på ett visst mönster, och i den här bloggposten kommer vi att titta närmare på hur man kan göra det i Rust.

## Hur man gör det
För att radera tecken som matchar ett visst mönster i Rust, kan man använda sig av metoden `retain` som finns tillgänglig för strängar. Denna metod tar emot ett closure som argument, och kommer att behålla alla tecken som matchar det mönster som definieras i closure:t. Om man till exempel vill ta bort alla siffror från en sträng, kan man göra såhär:

```Rust
let mut text = String::from("Rust är 1världs 2bästa 3prog1rammeringsspråk!");
text.retain(|c| !c.is_digit(10));
println!("{}", text); // Output: Rust är världs bästa programmeringsspråk!
```

Här används `retain`-metoden tillsammans med `is_digit`-metoden, som returnerar `true` för alla tecken som är siffror. Genom att negarera detta med `!` kommer `retain` att ta bort alla siffror från strängen och behålla resten.

## Deep Dive
I exemplet ovan användes `retain` för att ta bort siffror från en sträng, men det finns många fler möjligheter att utforska. Man kan till exempel använda sig av reguljära uttryck för att definiera mer avancerade mönster att matcha. Man kan även kombinera flera closures i en kedja, för att utföra flera olika raderingsoperationer på en och samma sträng. Det är även möjligt att använda sig av `drain`-metoden för att ta bort vissa tecken baserat på deras index i strängen. 

Det finns också andra metoder för att ta bort tecken från strängar, så som `replace` som byter ut tecken som matchar ett visst mönster mot en angiven sträng. Oavsett vilken strategi man väljer är det viktigt att förstå hur man kan manipulera strängar i Rust, och att ha koll på vilka olika metoder som finns tillgängliga.

## Se även
- Rust Documentation: [String](https://doc.rust-lang.org/std/string/struct.String.html)
- Rust By Example: [String](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- Regular Expressions in Rust: [Regex](https://docs.rs/regex/1.4.2/regex/)