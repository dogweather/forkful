---
title:                "HTML:n jäsentäminen"
html_title:           "C#: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan joutunut työskentelemään verkkosivujen kanssa ja tarvinnut tietoa tietystä elementistä? HTML-parsiminen mahdollistaa sivustojen sisällön analysoinnin ja tiedon hakemisen helposti ja nopeasti.

## Kuinka

HTML-parsimista varten voit käyttää C#-kielen [HtmlAgilityPackia](https://html-agility-pack.net/). Se tarjoaa kätevän tavan ladata ja analysoida HTML-tiedostoja.

Seuraava koodiesimerkki näyttää kuinka ladata HTML-sivu ja tulostaa kaikki sen otsikkoelementit:

```C#
var html = new HtmlDocument();
html.Load("https://www.example.com/");

var headings = html.DocumentNode.Descendants("h1");
foreach(var heading in headings)
{
    Console.WriteLine(heading.InnerText);
}

// Output:
// Example Website Title
// First Heading
// Second Heading
```

Voit myös parsia HTML-tiedoston tai sivun tiettyä elementtiä käyttämällä sen ID:tä tai luokkaa:

```C#
var elementWithId = html.GetElementById("elementId");
var elementsWithClass = html.DocumentNode.SelectNodes("//div[@class='className']");

// Output:
// <div id="elementId">Content</div>
// <div class="className">Content 1</div>
// <div class="className">Content 2</div>
```

## Syvällisempi sukellus

HTML-parsiminen mahdollistaa myös datan keräämisen ja manipuloinnin verkkosivuilla. Voit esimerkiksi hakea ja tallentaa tiettyjä tietoja sivulta tietokantaan.

Hyödyntämällä CssSelectoria voit parsia haluamiesi elementtien CSS-pohjalta:

```C#
var elements = html.DocumentNode.SelectNodes("cssSelector");

foreach(var element in elements)
{
    // Code to manipulate or store data
}
```

HTML-parsiminen on hyödyllinen taito, jota on helppo oppia ja käyttää. Se auttaa tehostamaan erilaisia verkkosivujen ja sovellusten kehitysprojekteja.

## Katso myös

- [HtmlAgilityPackin dokumentaatio](https://html-agility-pack.net/documentation)
- [C#-kielen virallinen sivusto](https://docs.microsoft.com/en-us/dotnet/csharp/)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään HTML-parsimisen tärkeyttä ja käyttöä C#-ohjelmoinnissa.