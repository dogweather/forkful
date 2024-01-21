---
title:                "Inviare una richiesta http"
date:                  2024-01-20T17:59:03.258985-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Inviare una richiesta HTTP significa chiedere dati a un server web. I programmatori lo fanno per interagire con API, scaricare contenuti o comunicare con altri servizi.

## How to: (Come Fare:)
Ecco un esempio usando `C++` e la libreria `C++ Requests` (cpr), che semplifica il processo:

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    // Invia una richiesta GET
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});

    // Stampa lo stato di risposta e il contenuto
    std::cout << "Status code: " << r.status_code << std::endl;
    std::cout << "Response: " << r.text << std::endl;

    return 0;
}
```
Output:
```
Status code: 200
Response: {
  ...
  "url": "http://httpbin.org/get"
}
```

## Deep Dive (Approfondimento)
Inviare richieste HTTP non è sempre stato semplice in C++. Prima delle librerie moderne, bisognava occuparsi di connessioni di basso livello e protocolli. Altri metodi includono l'uso di `libcurl` o `Boost.Asio` per un controllo più granulare. La libreria `C++ Requests` (cpr), basata su `libcurl`, fornisce un'API semplice e moderna. Ricorda che gestire le richieste in modo sincrono può bloccare il tuo programma; valuta l'uso di async/await o di un modello di concorrenza per evitare questo problema.

## See Also (Vedi Anche)
- C++ Requests (cpr) GitHub: https://github.com/libcpr/cpr
- libcurl: https://curl.se/libcurl/
- Boost.Asio: https://www.boost.org/doc/libs/release/libs/asio/