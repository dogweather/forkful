---
title:                "Rust: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

In questo articolo, scopriremo come inviare una richiesta HTTP con autenticazione di base utilizzando il linguaggio di programmazione Rust. Capiremo il motivo per cui questa funzione è utile e come implementarla correttamente nei nostri progetti.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Rust, possiamo utilizzare la libreria `reqwest`. Ecco un esempio di codice che mostra come inviare una richiesta GET con autenticazione di base a un determinato URL:

```Rust
use reqwest::header::HeaderValue;
use reqwest::StatusCode;

let client = reqwest::Client::new();
let url = "https://www.example.com";
let username = "username";
let password = "password";

// Creiamo un'header di autenticazione di base
let auth = HeaderValue::from_str(&(username.to_owned() + ":" + password)).unwrap();

// Inviamo la richiesta GET alla URL specificata
let response = client.get(url)
    // Impostiamo l'header di autenticazione appena creato
    .header(reqwest::header::AUTHORIZATION, auth)
    .send()
    .unwrap();

// Verifichiamo lo status code della risposta
if response.status() == StatusCode::OK {
    // La richiesta è andata a buon fine, possiamo leggere la risposta
    let body = response.text().unwrap();
    println!("{}", body);
} else {
    // La richiesta ha avuto uno status code diverso da 200
    println!("La richiesta ha restituito uno status code di errore: {}", response.status());
}
```

L'output di questo esempio sarà il corpo della risposta del server all'URL specificato.

## Approfondimento

Ora che sappiamo come inviare una richiesta HTTP con autenticazione di base in Rust, approfondiamo un po' su questo argomento. L'autenticazione di base è uno dei metodi di autenticazione più semplici ed è comunemente utilizzato per proteggere determinate risorse su un server. In pratica, il client invia un'header di autenticazione contenente nome utente e password codificati in base64 al server. Il server, a sua volta, verifica questi dati per determinare se il client ha il permesso di accedere alla risorsa richiesta.

Oltre alla libreria `reqwest`, ci sono altre opzioni per inviare richieste HTTP con autenticazione di base in Rust, come ad esempio `hyper` o `rustful`.

## Vedi anche

- Documentazione ufficiale di Rust su `reqwest`: https://docs.rs/reqwest/latest/reqwest/
- Esempi di autenticazione di base con `hyper`: https://hyper.rs/guides/client/auth/
- Documentazione su `rustful` con esempi di autenticazione di base: https://docs.rs/rustful/0.10.0/rustful/headers/struct.Authorization.html#examples