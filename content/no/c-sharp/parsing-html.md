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

## Hvorfor

Parsing av HTML er en vanlig oppgave for utviklere som jobber med å hente og behandle data fra nettsider. Dette kan være nyttig for å lage automatiserte prosesser, samle informasjon til en database eller for å gjøre endringer i eksisterende HTML-kode.

## Hvordan

For å hjelpe deg med å få en dyptgående forståelse av hvordan man parser HTML ved hjelp av C#, vil vi gå gjennom et enkelt eksempel og forklare det steg for steg.

Først må vi inkludere `System.Net` og `System.IO` i toppen av C#-filen vår:

```
using System.Net;
using System.IO;
```

Deretter kan vi bruke `WebRequest`-klassen for å sende en HTTP-forespørsel til en nettside:

```
WebRequest request = WebRequest.Create("https://www.example.com");
```

Nå må vi lese html-koden som responsen inneholder. Dette kan gjøres ved å bruke `GetResponse()`-metoden og `StreamReader`-klassen for å lese responsen:

```
WebResponse response = request.GetResponse();
StreamReader reader = new StreamReader(response.GetResponseStream());
string html = reader.ReadToEnd();
```

Etter å ha fått tak i HTML-koden, vil vi bruke en `HtmlDocument`-klasse fra `HtmlAgilityPack` for å analysere og manipulere koden. Her er et eksempel på hvordan man kan hente ut alle linkene på siden:

```
HtmlDocument doc = new HtmlDocument();
doc.LoadHtml(html);
HtmlNodeCollection links = doc.DocumentNode.SelectNodes("//a[@href]");

foreach(HtmlNode link in links)
{
    string url = link.GetAttributeValue("href", string.Empty);
    // gjør noe med url-en, for eksempel lagre den i en database
}
```

## Dypdykk

HTML-parsing kan være en kompleks prosess, spesielt hvis koden inneholder feil og mangler. For å hjelpe med dette, finnes det flere tredjeparts biblioteker som `HtmlAgilityPack`, `AngleSharp` og `CsQuery`.

Det er også viktig å merke seg at siden HTML-kode er uforutsigbar, kan det være vanskelig å skrive en parser som fungerer 100% av tiden. Derfor er det viktig å skrive feilhåndteringslogikk for å unngå å få programmet til å krasje.

## Se også

- [HtmlAgilityPack dokumentasjon](https://html-agility-pack.net/documentation)
- [AngleSharp repository](https://github.com/AngleSharp/AngleSharp)
- [CsQuery hjemmeside](https://www.csquery.com/)