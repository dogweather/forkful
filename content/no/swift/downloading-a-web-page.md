---
title:                "Swift: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside er en viktig del av å programmere Swift, spesielt for de som ønsker å lage apper som kan hente informasjon fra internett. Dette lar deg hente data fra en nettside og bruke den i din egen app.

## Hvordan

For å laste ned en nettside i Swift, kan du bruke `URLSession` og `Data` klassene. Først må du konvertere URL-en til en `URL` objekt, deretter kan du bruke `URLSession.shared.dataTask(with:completionHandler:)` metoden for å starte en datahentingsoperasjon. Her er et eksempel på hvordan du kan laste ned en nettside og skrive ut innholdet i konsollen:

```Swift
if let url = URL(string: "https://www.swiftblog.com") {
    URLSession.shared.dataTask(with: url) { (data, response, error) in
        if let data = data {
            print(String(data: data, encoding: .utf8))
        }
    }.resume()
}

```

Dette vil sende en forespørsel til nettstedet, hente dataene og skrive dem ut i konsollen. Du kan også bruke `URLSession` for å laste ned bilder eller andre filer fra en nettside.

## Deep Dive

Når du bruker `URLSession` for å laste ned en nettside, har du også kontroll over andre aspekter ved forespørselen, som for eksempel å legge til HTTP-headerfelt eller å sette en timeout-verdi. Du kan også bruke denne metoden for å laste ned data fra API-er eller internettbaserte tjenester til din app.

## Se Også

* [Apple Developer - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
* [How to Use URLSession to Make Web Requests in Swift](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)