---
title:                "Elixir: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med Elixir-programmering, er det ofte nødvendig å kommunisere med andre tjenester og API-er ved hjelp av HTTP. For å sikre at disse kommunikasjonene er trygge, kan det være nødvendig å sende en HTTP-forespørsel med grunnleggende autentisering. Dette sikrer at kun autoriserte brukere får tilgang til tjenesten, og beskytter sensitiv informasjon.

## Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering, må du først opprette en mappe som inneholder forespørselsinformasjon. Dette inkluderer URL-en til tjenesten, nødvendige parametere og eventuelle kroppsdeler. Deretter kan du bruke Elixir sin innebygde HTTP-client, `HTTPoison`, til å sende forespørselen.

```elixir
# Opprett en mappe med forespørselsinformasjon
request = %{
   url: "https://example.com/api",
   method: :get,
   headers: ["Authorization": "Basic <base64 encoded credentials>"],
   params: %{type: "article", id: 123}
}

# Send forespørselen og lagre svaret
response = HTTPoison.get(request.url, [], request.headers, request.params)

# Skriv ut svaret
IO.inspect response.body # => "Data fra tjenesten"
```

Vær oppmerksom på at brukernavnet og passordet må kodes til base64 format i autoriseringshodet før det sendes som en del av forespørselen.

## Dypdykk

HTTP-forespørsler med grunnleggende autentisering bruker en HTTP-header kalt "Authorization" som inneholder informasjon om brukernavn og passord. Dette er en vanlig metode for autentisering, men det er viktig å merke seg at informasjonen som sendes er kryptert og ikke garantert å være sikker.

For å sikre kommunikasjonen ytterligere, kan det være lurt å se på andre autentiseringsmetoder som OAuth eller API-nøkler. Disse gir en mer robust og sikker måte å autentisere HTTP-forespørsler på.

## Se Også

- Offisiell Elixir dokumentasjon om HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- En grundig guide om å bruke HTTP i Elixir: https://www.poeticoding.com/securing-api-requests-using-basic-authentication-in-elixir/