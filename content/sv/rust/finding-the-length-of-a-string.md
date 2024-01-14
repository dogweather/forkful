---
title:    "Rust: Att hitta längden på en sträng"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet inom programmering, oavsett vilket språk man använder. Detta är särskilt viktigt inom Rust, ett språk som fokuserar på både prestanda och säkerhet. Genom att förstå hur man hittar längden på en sträng i Rust, kan man optimera sin kod och undvika potentiella säkerhetsproblem.

## Hur man gör

För att hitta längden på en sträng i Rust, kan man använda sig av bult-in funktionen "len()" eller "os::raw::strlen()". Dessa funktioner tar emot en sträng som argument och returnerar längden på strängen i form av ett heltal.

```Rust
let s: &str = "Hej, mitt namn är Rust!";
let length = s.len(); // Ger längden på strängen som är 23

let c_str: *const c_char = CString::new(s).unwrap().as_ptr();
let c_length = unsafe { strlen(c_str) }; // Ger också längden på strängen som är 23
```

Att veta längden på en sträng kan vara användbart när man behöver skapa en loop för att gå igenom varje tecken i strängen eller när man behöver klona eller manipulera strängen på olika sätt. Detta underlättar även när man arbetar med datastrukturer som vektorer eller listor, där man behöver ange antal element som ska behandlas.

## Djupdykning

Att förstå hur längden på en sträng beräknas i Rust kan hjälpa till att undvika potentiella programfel. Strängar i Rust är UTF-8-kodade, vilket betyder att varje tecken i strängen kan ta upp olika mängd bytes beroende på dess kodposition. Detta påverkar även längden på strängen, då "len()" funktionen räknar antalet bytes i en sträng istället för antalet tecken.

Detta kan leda till problem om man inte är medveten om det och räknar felaktigt med antalet tecken i strängen. Därför är det viktigt att använda "char()" funktionen för att räkna antalet tecken istället för "len()" funktionen om man vill ha en mer exakt längd på strängen.

## Se även

- [Officiell Rust dokumentation för strängar](https://doc.rust-lang.org/std/string/index.html)
- [Rust Book: strängar](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Book: tecken och strängar](https://doc.rust-lang.org/book/ch04-03-slices.html#string-slices)