---
title:                "Å sende en http-forespørsel"
html_title:           "Rust: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å sende en HTTP-forespørsel er en måte å kommunisere med en webserver på. Dette er en vanlig oppgave for programmerere, da det tillater dem å hente data fra en webserver og bruke den i sine programmer.

# Hvordan:

```Rust
use reqwest::blocking::get;

let response = get("https://www.example.com").unwrap();
println!("Response status code: {}", response.status());
println!("Response headers:\n{:?}", response.headers());
println!("Response body:\n{}", response.text().unwrap());
```

Output:
```
Response status code: 200 OK
Response headers:
{"content-type": "text/html"}
Response body:
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
</html>
```

# Dypdykk:

HTTP (Hypertext Transfer Protocol) er et protokoll som brukes for å sende informasjon mellom en klient (for eksempel en nettleser) og en server (for eksempel en webside). Det er flere andre alternativer til å sende HTTP-forespørsler i Rust, som for eksempel Hyper og Surf biblioteker.

Implementasjonen av å sende HTTP-forespørsler i Rust er enkelt takket være de mange nyttige bibliotekene som finnes. Disse bibliotekene håndterer alle detaljer som å opprette TCP-tilkoblinger og håndtere Request and Response objekter.

# Se også:

- Rust's offisielle documentation om HTTP-forespørsler: [https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- Eksempler på å sende HTTP-forespørsler i Rust: [https://github.com/alexcrichton/twitch-rs/blob/master/src/api.rs](https://github.com/alexcrichton/twitch-rs/blob/master/src/api.rs)