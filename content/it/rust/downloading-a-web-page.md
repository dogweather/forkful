---
title:                "Scaricare una pagina web"
html_title:           "Rust: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Scaricare una pagina web è il processo di ottenere il contenuto di una pagina web da Internet e memorizzarlo sul proprio dispositivo. I programmatori spesso lo fanno per accedere a dati specifici da una pagina web o per creare script che automatizzano azioni su una pagina web.

## Come fare:

```Rust
// Importiamo il modulo "reqwest" per effettuare richieste HTTP
use reqwest;

// Utilizziamo la funzione "get" per ottenere il contenuto di una pagina web
let response = reqwest::get("https://www.example.com").await?;

// Se la richiesta ha successo, stampiamo il contenuto della pagina
if response.status().is_success() {
    println!("{}", response.text().await?);
}
```

Output:

```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>
<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Approfondimento:

Scaricare una pagina web è diventato un'operazione sempre più importante per i programmatori con l'avvento del web e delle applicazioni web. Una volta, dovevano essere utilizzati script di scripting come Python o PHP per scaricare il contenuto di una pagina web. Ora, con Rust, è possibile scrivere codice più efficiente e sicuro per ottenere i contenuti di una pagina web utilizzando librerie di alto livello come "reqwest".

## Vedi anche:

- [Pagina GitHub di "reqwest"](https://github.com/seanmonstar/reqwest) per maggiori informazioni su come utilizzare la libreria.
- [Rustlings](https://github.com/rust-lang/rustlings) per imparare di più su Rust attraverso esercizi pratici.