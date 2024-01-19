---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en webside er handlingen av å hente alle dataene fra en spesifikk nettadresse gjennom sikkert HTTP (Hypertext Transfer Protocol). Programmere gjør det for å analysere, manipulere eller lagre innholdet til senere bruk.

## Hvordan:

Vi bruker `gleam/httpc` biblioteket for å laste ned en nettside. 

Installere biblioteket ved å legge dette til `rebar.config` filen:

```Gleam
{deps, [
    {httpc, "0.1.0"}
]}.
```

Deretter, bruk funksjonen `httpc.get` for å laste ned websiden:

```Gleam
import gleam/httpc
import gleam/string

pub fn hent_webside() {
  let respons = httpc.get("https://eksempel.com").send()
  case respons {
    Ok(body) -> string.from_slice(body)
    Error(err) -> io.println(err)
  }
}
```

Denne koden vil skrive ut innholdet på "https://eksempel.com" eller feilmeldingen hvis den ikke klarer å laste ned sidene.

## Dypdykk

Historisk sett, opprinnelig var nedlasting av nettsider begrenset til bruk i nettlesere. Men med fremgang av backend-teknologier, er det nå vanlig å last ned webside data for andre formål som webskraping, testing og mer.

Alternativer til `gleam/httpc` inkluderer andre biblioteker som `httpc` eller `hackney`. Noen forskjeller kan inkludere ytelse, feilhåndtering, og funksjonalitet tilgjengelig, avhengig av biblioteket.

Når det gjelder selve implementeringen, oppretter `httpc.get` en GET-forespørsel til den angitte nettadressen, sender forespørselen, og returnerer svaret som en `Result`(resultat).

## Se Også 

Her er noen nyttige lenker til relaterte emner:

1. Gleam HTTP-klientdokumentasjon: [Gleam HTTP](https://hexdocs.pm/gleam_http/)
2. HTTP-protokollen: [HTTP/1.1: Semantics and Content](https://tools.ietf.org/html/rfc7231)
3. Gleam programmeringsspråkdokumentasjon: [Gleam Language](https://gleam.run/docs/)
4. Veiledning for nettverskraping: [Web Scraping Tutorial](https://www.datacamp.com/community/tutorials/web-scraping-tutorial-python)