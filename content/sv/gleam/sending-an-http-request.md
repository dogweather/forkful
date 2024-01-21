---
title:                "Skicka en http-förfrågan"
date:                  2024-01-20T17:59:46.736749-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan innebär att man ber en webbserver om information eller att man skickar data till den. Programmerare gör detta för att interagera med webbtjänster och utbyta data mellan klienter och servrar.

## Hur gör man:
```gleam
import gleam/http
import gleam/httpc

pub fn main() {
  let response = httpc.send(req("https://api.example.com/data", http.Get))
  case response {
    Ok(response) -> io.println("Fick svar: " ++ response.body)
    Error(error) -> io.println("Något gick fel: " ++ error)
  }
}
```

Exempel på output:
```
Fick svar: {"name": "Gleam", "type": "programming language"}
```

## Djupdykning
HTTP-förfrågningar har varit en grundläggande del av webbutveckling sedan webbens början. I Gleam används modulerna `http` och `httpc` för att skicka förfrågningar. Det finns alternativ som `gleam_http` och externa bibliotek som `reqwest` för Erlang och Elixir. Implementeringsdetaljer kan variera, men principen är universell – skicka en förfrågan, hantera svaret.

## Se också
- Gleam HTTP documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Erlang's httpc module documentation for deeper understanding: [http://erlang.org/doc/man/httpc.html](http://erlang.org/doc/man/httpc.html)