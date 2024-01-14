---
title:                "Rust: Sända en http-förfrågan"
simple_title:         "Sända en http-förfrågan"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
Många moderna applikationer är beroende av att kunna skicka HTTP-förfrågningar för att kommunicera med externa API:er eller webbservrar. I denna artikel kommer du lära dig hur du kan göra det med hjälp av Rust-programmeringsspråket.

## Hur du gör
Att skicka en HTTP-förfrågan i Rust kan verka överväldigande till en början, men med rätt verktyg och kunskap blir det enkelt. Såhär kan du gå från en grundläggande förfrågan till en mer komplex interaktion:

```Rust
// Importera biblioteket "reqwest" som hjälper till med HTTP-förfrågningar
use reqwest::blocking::get; // I detta fall kommer vi använda "blocking" funktionen för att få en enklare kodstruktur

// Definiera URL att skicka förfrågan till
let url = "https://www.example.com"; // Byt ut mot den URL du vill använda

// Skapa en variabel som sparar förfrågningens resultat
let response = get(url)?.text().unwrap();

// Validera resultatet
assert!(response.contains("Welcome to Example")); // Om den mottagna texten innehåller "Welcome to Example" kommer testet att passera, annars kommer det att misslyckas
```

Detta är en mycket grundläggande kodstruktur för att skicka en HTTP-förfrågan och få ett svar. Du kan också lägga till logik för autentisering, hantering av olika förfrågningsmetoder som GET och POST, och hantering av olika HTTP-statuskoder.

## Deep Dive
En HTTP-förfrågan består av en "request line", "headers", och en "message body". Request line innehåller information om vilken metod som används (exempelvis GET eller POST), URL:n som förfrågan skickas till, och versionen av HTTP-protokollet. Headers är ytterligare information som kan inkluderas, såsom autentisering eller specifika instruktioner för servern. Message body är den faktiska datan som skickas i förfrågan, till exempel en JSON-sträng eller en textfil.

Det finns olika bibliotek som kan hjälpa dig att bygga upp din HTTP-förfrågan beroende på dina behov. I exemplet ovan använde vi "reqwest", men andra populära bibliotek inkluderar "hyper" och "reqwest async".

## Se även
- [Rust programmeringsspråk](https://www.rust-lang.org/sv)
- [Introduction to HTTP in Rust with Hyper](https://medium.com/@shnupta/introduction-to-http-in-rust-with-hyper-6db27f970dc7)
- [Rust By Example - HTTP Clients](https://doc.