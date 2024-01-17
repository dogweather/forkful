---
title:                "HTML-analys"
html_title:           "Rust: HTML-analys"
simple_title:         "HTML-analys"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsra HTML är en viktig del av webbutveckling eftersom det är det språk som används för att skapa och strukturera webbsidor. Genom att parsra HTML kan programmerare bearbeta och manipulera innehållet på en webbsida för att göra den mer dynamisk och användarvänlig.

## Så här gör du:

För att parsra HTML i Rust kan du använda biblioteket `html5ever`. Nedan finns ett enkelt exempel på hur man kan använda detta bibliotek för att hitta och skriva ut länkarna på en webbsida:
```Rust
// Ladda in biblioteket
extern crate html5ever;

// Skapa en variabel med en webblänk
let url = "https://example.com";

// Parsra HTML från länken
let new_html = html5ever::parse(url);

// Loopa igenom alla länkar på sidan och skriv ut dem
for link in new_html.find_all(links) {
    println!("{}", link);
}
```

Exempelutdata:
```
https://example.com/about
https://example.com/contact
https://example.com/products
```

## Djupdykning:

En intressant historisk kontext är att HTML utvecklades av Tim Berners-Lee på 1990-talet som en del av hans arbete på CERN. Alternativ till att parsra HTML är att använda andra språk som XHTML eller XML. En viktig del av att parsra HTML är att förstå DOM-trädet, som är den hierarkiska strukturen för hur HTML-dokumentet är uppbyggt.

## Se även:

- [Officiell dokumentation för html5ever](https://docs.rs/html5ever/0.22.0/html5ever/)
- [Dokumentation för DOM-trädet i HTML](https://www.w3.org/TR/REC-DOM-Level-1/level-one-html.html)