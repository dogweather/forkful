---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Rust: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione di base è un metodo comune utilizzato dai programmatori per accedere a risorse protette su una rete, come ad esempio una password protetta o una pagina web privata.

Come:
```Rust
extern crate reqwest;

use std::io::Read;

fn main() {
    let mut response = reqwest::get("http://example.com")
        .expect("Errore nella richiesta HTTP")
        .text()
        .expect("Errore nel parsing della risposta");
        
    println!("Response: {}", response);
}
```
Questo codice utilizza la libreria di terze parti "reqwest" per inviare una richiesta HTTP al sito "example.com" e ottenere una risposta sotto forma di testo.

Deep Dive:
L'autenticazione di base è uno dei metodi di autenticazione più antichi utilizzati sul web. Essenzialmente, il client invia una stringa contenente il nome utente e la password, codificati in base64, all'interno dell'header "Authorization" di una richiesta HTTP. Questo metodo di autenticazione non è considerato sicuro poiché la stringa può essere facilmente decodificata, ma è ancora ampiamente utilizzato per i suoi scopi di compatibilità e facilità d'uso.

Alternative:
Ci sono molti altri metodi di autenticazione che offrono un livello maggiore di sicurezza, come ad esempio l'autenticazione a chiave pubblica. Tuttavia, l'autenticazione di base viene utilizzata spesso per risorse interne o in situazioni in cui la sicurezza non è una preoccupazione principale.

Implementazione:
Come mostrato nell'esempio di codice, l'utilizzo della libreria "reqwest" semplifica notevolmente il processo di invio di una richiesta HTTP con autenticazione di base. Tuttavia, è possibile anche impostare manualmente l'header "Authorization" utilizzando altre librerie come ad esempio "hyper".

Vedi anche:
- Documentazione della libreria "reqwest": https://docs.rs/reqwest
- Altro codice di esempio per l'autenticazione di base in Rust: https://github.com/berdasarkan/rust-http-basic-auth-example