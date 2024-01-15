---
title:                "Parsning av HTML"
html_title:           "Gleam: Parsning av HTML"
simple_title:         "Parsning av HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har behövt extrahera specifika data från en webbsida, så har du förmodligen mött det frustrerande problemet med att behöva gå igenom en stor mängd HTML-kod. Men med Gleam, behöver du inte längre oroa dig för att manuellt bearbeta koden. Med hjälp av Gleam's HTML-parsing funktion kan du snabbt och enkelt extrahera den data du behöver från en webbsida.

## Så här gör du

För att börja parsningen av en HTML-sida, måste du först installera och importera modulen "html". För att göra detta, använd kommandot ```mix add html``` i din projektmapp och importera modulet genom att lägga till "use html" i början av dina kodfiler.

Nästa steg är att ladda in HTML-koden som du vill parsar. Du kan göra detta genom att använda funktionen "load" som finns i html-modulet. Detta gör att koden sparas i en variabel, som du sedan kan använda för att extrahera data från.

```Gleam
let html_data = html.load("<html><body><h1>Welcome to Gleam!</h1></body></html>")
```

Nu kan vi börja extrahera data genom att använda funktioner som "get_text" och "get_attribute". Dessa tar in en HTML-element och returnerar antingen dess textinnehåll eller värde för en specifik attribut.

```Gleam
let heading = html.get_text(html_data |> html.find("h1"))

let paragraph = html_data
  |> html.find(".paragraph")
  |> html.get_attribute("class")
```

Slutligen kan vi använda "find_all" för att hämta flera element med samma tag eller klassnamn.

```Gleam
let links = html_data |> html.find_all("a")

links |> List.iter(html.get_attribute("href") |> debug.log)
```

## Djupdykning

Till skillnad från regex, som ofta används för att parsea HTML, så använder Gleam en mer "smart" metod som tar hänsyn till strukturen av koden. Detta betyder att du kan vara säker på att du får exakt den data du söker efter.

I Gleam, så representeras HTML-element som skräddarsydda typer för att säkerställa att du bara åtkommer giltig data. Detta eliminierar behovet av flera kontroller och felhanteringar.

HTML-parsing i Gleam är också väldigt snabbt tack vare dess effektiva algoritmer. Detta är särskilt användbart om du behöver parsar stora mängder HTML-kod.

## Se även

- [Officiell Gleam dokumentation för HTML-modulen](https://gleam.run/modules/html.html)
- [GitHub repository för HTML-modulen](https://github.com/gleam-lang/html)
- [Demo applikation med HTML-parsing i Gleam](https://github.com/spencerlefant/html-parsing-demo)