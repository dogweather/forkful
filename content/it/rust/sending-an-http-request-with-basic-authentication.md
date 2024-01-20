---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Invio di una richiesta HTTP con autenticazione base significa fornire un nome utente e una password per l'accesso al server. I programmatori lo fanno per garantire la sicurezza e restringere l'accesso a particolari risorse sul server.

## Come fare:
In Rust, potresti usare la libreria 'reqwest'. Ecco un esempio:

```Rust
use reqwest::Client;
use reqwest::header::HeaderValue;

let client = Client::new();
let url = "http://example.com";
let auth_value = HeaderValue::from_str(&format!("Basic {}", base64::encode("username:password")))
    .unwrap();

let res = client.get(url)
    .header(reqwest::header::AUTHORIZATION, auth_value)
    .send()
    .await?;

println!("Response: {:?}", res);
```

Questo codice invia una richiesta GET ad un URL con l'autenticazione base. Nella risposta, vedrai se l'accesso è stato concesso o no.

## Approfondimento:
L'autenticazione base evita l'accesso non autorizzato, ma non è sicura come altre tecniche, poiché trasmette le credenziali come una stringa codificata in Base64, che può essere facilmente decodificata. Entrò in uso nei primi giorni dell'Internet.

Esistono alternative più sicure, come l'autenticazione Digest o l'autenticazione con token JWT. Inoltre, l'autenticazione OAuth è ampiamente usata per le API Web.

Implementare l'autenticazione base in Rust implica la codifica di una coppia username e password in Base64 e l'invio di questa stringa nell'header di autorizzazione della richiesta HTTP.

## Vedi anche:
1. [Rust Reqwest Doc](https://docs.rs/reqwest/0.10.4/reqwest/)
2. [HTTP Basic Auth](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
3. [Rust Base64](https://docs.rs/base64/0.9.3/base64/)