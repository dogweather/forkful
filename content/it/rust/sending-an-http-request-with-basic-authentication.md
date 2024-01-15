---
title:                "Inviare una richiesta http con autenticazione di base."
html_title:           "Rust: Inviare una richiesta http con autenticazione di base."
simple_title:         "Inviare una richiesta http con autenticazione di base."
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Non c'è modo più semplice per autenticarsi in una richiesta HTTP che utilizzare l'autenticazione di base (basic). È fondamentalmente una nome utente e una password codificati in base64 all'interno dell'header della richiesta, ed è un metodo di autenticazione ampiamente supportato dalle API e dai servizi web.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Rust, abbiamo bisogno di utilizzare una libreria esterna chiamata `reqwest`, che fornisce un modo semplice e intuitivo per effettuare richieste HTTP e gestire le relative risposte.

```Rust
use reqwest;

let client = reqwest::Client::new();
let response = client.get("https://example.com/api")
    .basic_auth("username", Some("password"))
    .send()
    .await?;

println!("Status code: {}", response.status());
println!("Headers: \n{:?}", response.headers());
println!("Body: {}", response.text().await?);
```

Ecco un semplice esempio di come utilizzare `reqwest` per inviare una richiesta GET all'API di un sito di esempio con l'autenticazione di base. Possiamo anche specificare le credenziali del nostro account tramite il metodo `basic_auth()` o utilizzare un `match` per gestire il token di autenticazione ricevuto dalla risposta.

## Approfondimento

Ci sono alcune cose importanti da considerare riguardo l'utilizzo dell'autenticazione di base in una richiesta HTTP. In primo luogo, è importante notare che le credenziali vengono inviate in chiaro e possono essere facilmente intercettate da un utente malintenzionato. Pertanto, è sempre consigliabile utilizzare l'autenticazione di base in combinazione con una connessione HTTPS sicura.

Inoltre, mentre utilizzare l'autenticazione di base è una soluzione rapida e semplice, non è necessariamente la più sicura. Le credenziali sono codificate in base64, che è un processo di codifica non sicuro e può essere decodificato facilmente. Quindi, se la sicurezza è una preoccupazione, considera l'utilizzo di un metodo di autenticazione più sicuro come OAuth o token di accesso.

## Vedi anche

- [Documentazione di `reqwest`](https://docs.rs/reqwest/)
- [Guida alla sicurezza delle richieste HTTP in Rust](https://blog.logrocket.com/http-security-for-rust-web-apps/)
- [Articolo su come utilizzare OAuth in Rust](https://www.codementor.io/blog/rust-oauth-clients-authentication-services-2tboqk355s)