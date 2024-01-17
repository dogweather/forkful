---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Elixir: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering betyr å legge til brukernavn og passord i en forespørsel for at serveren skal kunne verifisere identiteten til forespørselen. Dette er vanligvis gjort for å sikre at bare autoriserte brukere kan få tilgang til en ressurs eller utføre en handling.

# Hvordan:
Eksempel 1:
```Elixir
response = HTTPotion.get("https://example.com/api/users", auth: {"username", "password"})
```
Output:
Et svar som inneholder informasjon om brukerne som ble hentet fra API-en.

Eksempel 2:
```Elixir
response = HTTPotion.get("https://example.com/api/posts", headers: ["authorization": Basic.encode64("username:password")])
```
Output:
Et svar som inneholder informasjon om innlegg som ble hentet fra API-en.

# Dypdykk:
Grunnleggende autentisering er en enkel form for autentisering som har blitt brukt i lang tid på internett. Det er imidlertid en sikkerhetsrisiko, da brukernavn og passord kan bli avlyttet og stjålet. En alternativ metode for autentisering er OAuth, som bruker tokens i stedet for brukernavn og passord. Implementeringen av grunnleggende autentisering involverer å legge til en `Authorization` header med brukernavn og passord som er kodet med Base64.

# Se også:
- [HTTPotion biblioteket](https://hexdocs.pm/httpotion/HTTPotion.html) for å utføre HTTP-forespørsler i Elixir.
- [BaseX biblioteket](https://hexdocs.pm/basex/BaseX.html) for å kryptere tekst med Base64 i Elixir.
- [Elixir Style Guide](https://github.com/christopheradams/elixir_style_guide) for å lære mer om å skrive ren og lesbar Elixir-kode.