---
title:                "Rust: Att Göra en Sträng Stor"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Rust är ett modernt programmeringsspråk som har blivit alltmer populärt bland utvecklare. Det är ett säkert och effektivt språk som är väl lämpat för systemprogrammering och andra krävande applikationer. En av dess stora styrkor är dess starka typsystem som hjälper till att förhindra många vanliga fel. I denna bloggpost kommer vi att titta på en av de grundläggande funktionerna i Rust - att kapa en sträng (string).

## Hur man gör

För att kapa en sträng i Rust, kan du använda inbyggda funktionen "to_uppercase". Det här är ett exempel på hur du skulle använda den i ditt program:

```Rust
let namn = "jesper";
let kapitaliserat_namn = namn.to_uppercase();
```

I detta exempel skapar vi en variabel "namn" som innehåller en vanlig sträng - "jesper". Sedan använder vi "to_uppercase" funktionen för att skapa en ny variabel "kapitaliserat_namn" som innehåller samma sträng, men med alla bokstäver konverterade till versaler. 

## Djupdykning

Det finns flera andra sätt att kapa en sträng i Rust, såsom att använda "to_uppercase" metod för strängt: 

```Rust
let kapitaliserat_namn = "jesper".to_uppercase();
```

Du kan också kapa en del av en sträng genom att använda "get_mut" funktionen och ändra enskilda bokstäver till versaler: 

```Rust
let mut namn = String::from("jesper");
let bokstav = namn.get_mut(0..1).unwrap();
bokstav.make_ascii_uppercase();
```

Detta skulle resultera i variabeln "namn" som innehåller "Jesper", med den första bokstaven kapitaliserad. 

## Se också

- Rust språkwebbplats: https://www.rust-lang.org/sv-SE/
- Rust dokumentation: https://doc.rust-lang.org/stable/std/
- Rust subreddit: https://www.reddit.com/r/rust/