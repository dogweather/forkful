---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Invio di una richiesta HTTP è il processo con cui i nostri programmi chiedono informazioni alla rete. I programmatori lo usano per accedere a servizi web, scaricare dati, fare operazioni CRUD e molte altre cose.

## Come fare:

Ecco l'esempio di come si fa con `khttp`.

```Kotlin
import khttp.get

fun main() {
    val response = get("https://httpbin.org/get")
    println(response.statusCode)
    println(response.text)
}
```

Qui, basta importare il modulo `khttp` e usare il metodo `get`. Questo restituirà una risposta che ha un campo di statusCode e text.

## Approfondimento

Historicamente, le richieste HTTP sono tra le prime operazioni eseguite dai programmi per l'interazione con Internet. Oggigiorno, è possibile usare vari approcci come `HttpURLConnection`, altre librerie come `OkHttp` o il modulo `khttp`.

Come dettaglio di implementazione, le richieste HTTP seguono un modello di `request-response`. Invi un messaggio di richiesta, il server lo processa, e poi si riceve una risposta che può essere trattata a piacimento.

## Da Vedere Anche:

- Per saperne di più su `khttp` [clicca qui](https://github.com/jkcclemens/khttp).
- Documentazione sulle richieste HTTP [qui](https://developer.mozilla.org/it/docs/Web/HTTP/Overview).
- Per un tutorial più dettagliato su Kotlin e HTTP, [visita questo link](https://www.baeldung.com/kotlin-http-requests).