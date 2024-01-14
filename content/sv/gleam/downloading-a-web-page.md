---
title:                "Gleam: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför
Att ladda ner en webbsida kan vara användbart för att hämta information eller för att spara en kopia av en sida som du vill komma åt senare.

## Hur man gör det
Det första steget för att ladda ner en webbsida med Gleam är att importera "webpage" biblioteket. Därefter kan du använda funktionen "fetch" för att hämta en specifik URL.

```Gleam
let webpage = import("webpage")
let url = "https://www.example.com"
let response = webpage.fetch(url)
```

För att kunna använda resultatet från "fetch" funktionen behöver vi konvertera det till en "Result" typ. Detta kan göras med hjälp av "to_result" funktionen. Om allt går bra kommer du få en "Ok" med en "Webpage" typ som innehåller all information om den nedladdade webbsidan.

```Gleam
fun fetch(url) {
  webpage
    .fetch(url)
    .to_result()
}

// Output:
// Ok {
//   response: Webpage {
//     status: 200,
//     ...
//   }
// }
```

Om det blir några problem under hämtningen, till exempel att URL:en inte är giltig, kommer "to_result" funktionen returnera en "BadResponse" med information om vad som gick fel.

```Gleam
fun fetch(url) {
  webpage
    .fetch(url)
    .to_result()
}

// Output:
// BadResponse {
//   message: "Invalid URL",
//   status: 400,
//   ...
// }
```

## Djupdykning
Genom att använda "Webpage" typen som returneras från "fetch" funktionen kan du utforska det hämtade innehållet och hämta specifika delar av sidan. Till exempel kan du hämta titeln på en sida och alla dess länkar.

```Gleam
let title = webpage.title(response)
let links = webpage.links(response)

// Output:
// title: "Example Website"
// links: [
//   "https://www.example.com/page-1",
//   "https://www.example.com/page-2",
//   ...
// ]
```

Du kan också hämta hela HTML-koden för sidan genom att använda "body" funktionen.

```Gleam
let html = webpage.body(response)

// Output:
// "<html>...</html>"
```

## Se även
- [Gleam webpage bibliotek](https://gleam.run/packages/webpage/)
- [Webpage Dokumentation](https://gleam.run/packages/webpage/docs/)