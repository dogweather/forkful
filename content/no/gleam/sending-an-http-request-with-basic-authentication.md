---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Gleam: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

HTTP-forespørsler med grunnleggende autentisering er en viktig del av å kommunisere med eksterne tjenester og API-er. Dette tillater oss å sende og motta data på en sikker måte ved å autentisere oss mot tjenesten vi kommuniserer med.

## Slik gjør du det

Ettersom Gleam er et nytt programmeringsspråk, er det ikke mye informasjon der ute om hvordan man sender HTTP-forespørsler med grunnleggende autentisering. Men det er ganske enkelt å gjøre det i Gleam med hjelp av biblioteket [`ninenines/hackney`](https://github.com/ninenines/hackney). Følg de enkle trinnene nedenfor for å begynne å sende autentiserte HTTP-forespørsler i Gleam.

Først må vi legge til `ninenines/hackney` i `gleam.toml`-filen vår under `dependencies`-seksjonen:

```Gleam
[dependencies]
ninenines/hackney = "3.1.0"
```

Deretter kan vi importere [`hackney`](https://hexdocs.pm/hackney/readme.html) biblioteket i filen vår og sette opp et `httpClient`-objekt for å kommunisere med tjenesten vi vil sende en forespørsel til:

```Gleam
import hackney

httpClient = hackney:start()
```

Nå kan vi sende en HTTP-forespørsel med autentisering ved hjelp av `hackney:basic_auth()`-funksjonen. Den tar inn en URL, brukernavn og passord og returnerer et `Result`-objekt som enten vil være `Ok` med responsen fra tjenesten eller `Error` med en feilmelding. Her er et eksempel på hvordan dette kan se ut i Gleam:

```Gleam
hackney:basic_auth(
    httpClient,
    "https://example.com/api",
    "my_username",
    "my_password"
)
|> case _ {
    Ok(response) -> io:format("Got successful response: {}", [response])
    Error(error) -> io:format("Got error: {}", [error])
}
```

Dette vil sende en GET-forespørsel til `https://example.com/api` med autentiseringsinformasjonen som en del av headeren. Du kan også bruke [`hackney:basic_auth_headers()`](https://hexdocs.pm/hackney/readme.html#basic_auth_headers/2) hvis du vil legge til autentiseringen manuelt i headeren din. Se [`hackney`-dokumentasjonen](https://hexdocs.pm/hackney/readme.html#basic_auth/3) for mer informasjon.

## Dykk dypere

Det er også mulig å bruke [`hackney:basic_auth/3`](https://hexdocs.pm/hackney/readme.html#basic_auth/3) for å sende en spesifikk HTTP-metode (som POST eller PUT) med autentisering. Det er også viktig å merke seg at hvis tjenesten du kommuniserer med bruker en annen autentiseringsform enn grunnleggende autentisering, som OAuth, må du bruke en annen funksjon fra `hackney`-biblioteket. Så sørg for å lese dokumentasjonen nøye for å velge riktig funksjon for din situasjon.

## Se også

- [Gleam offisiell dokumentasjon](https://gleam.run/)
- [Hackney dokumentasjon](https://hexdocs.pm/hackney/readme.html)