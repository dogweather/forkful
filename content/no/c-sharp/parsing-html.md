---
title:                "Analysering av HTML"
html_title:           "C#: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML er en teknikk for å hente ut informasjon fra HTML-kode, som ofte brukes til å lage nettsider. Dette kan være nyttig for programmere som ønsker å automatisere oppgaver, som å samle data fra flere nettsider eller analysere strukturen på en nettside.

## Hvordan:
Her er et eksempel på hvordan man kan parse HTML-kode ved hjelp av C#:

```C#
using HtmlAgilityPack;
// Hent nettsiden du ønsker å parse:
var web = new HtmlWeb();
var doc = web.Load("https://www.example.com/");
// Finn alle lenker på nettsiden:
var links = doc.DocumentNode.SelectNodes("//a[@href]");
// Skriv ut alle lenkene:
foreach (var link in links)
{
    Console.WriteLine(link.Attributes["href"].Value);
}
```

Eksempel på output:
```
https://www.example.com/page1
https://www.example.com/page2
https://www.example.com/page3
```

## Dypdykk:
Historisk sett ble parsing av HTML gjort manuelt, men dette var tidkrevende og ga ofte unøyaktige resultater. I dag finnes det flere biblioteker og API-er som gjør parsing enklere og mer presist. Alternativer til C# inkluderer blant annet Python og JavaScript. Implementasjonen av parsing kan variere avhengig av hvilket bibliotek man bruker, men prinsippet er det samme: å analysere HTML-kode og hente ut ønsket informasjon.

## Se også:
- Offisiell dokumentasjon for HtmlAgilityPack biblioteket: https://html-agility-pack.net/
- En guide for å parse HTML med Python: https://realpython.com/beautiful-soup-web-scraper-python/
- En tutorial for å parse HTML med JavaScript: https://www.digitalocean.com/community/tutorials/how-to-parse-html-web-pages-with-jsdom-in-node-js