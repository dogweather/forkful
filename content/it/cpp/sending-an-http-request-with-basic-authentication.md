---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Arduino: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con autenticazione di base in C++

## Che cos'è e perché?
L'invio di richieste HTTP con autenticazione di base è un modo per le applicazioni di comunicare con i servizi web in modo sicuro. I programmatori lo utilizzano spesso per fornire un livello essenziale di sicurezza durante la trasmissione di dati su internet.

## Come si fa:
Qui c'è un esempio di come inviare una richiesta HTTP con autenticazione di base utilizzando la libreria `C++ REST SDK` chiamata `Casablanca`. 

```C++
#include <cpprest/http_client.h>
#include <cpprest/filestream.h>

using namespace utility;                    
using namespace web;                        
using namespace web::http;                  
using namespace web::http::client;          
using namespace concurrency::streams;       

int main(int argc, char* argv[])
{
    http_client client(U("http://example.com"));
    http_request request(methods::GET);

    request.headers().add(L"Authorization", L"Basic " + utility::conversions::to_base64(L"username:password"));

    client.request(request)
    .then([](http_response response) -> pplx::task<json::value>
    {
        return response.extract_json();
    })
    .then([](pplx::task<json::value> previousTask)
    {
        try
        {
            const json::value& v = previousTask.get();
            std::wcout << v.to_string() << std::endl;
        }
        catch (http_exception const & e)
        {
            std::wcout << e.what() << std::endl;
        }
    })
    .wait();

    return 0;
}
```

Nel codice, modificare `"username:password"` con le tue credenziali.

## Approfondimenti

L'autenticazione di base è una tecnica standard per la gestione dell'accesso sicuro ai servizi web, tuttavia, risale agli albori del web e la sua semplicità si riflette nel suo nome: è davvero piuttosto "di base". Questo metodo non fornisce un'ampia gamma di caratteristiche di sicurezza e si basa su username e password crittografate in base64.

Esistono alternative più sicure all'autenticazione di base, come l'autenticazione Digest e l'autenticazione delle informazioni di accesso OAuth2. Quest'ultima è l'opzione preferita per le applicazioni moderne grazie al suo livello di sicurezza avanzato e alla flessibilità. 

Tuttavia, per l'utilizzo di base e i test, l'autenticazione con credenziali di base si dimostra ancora un'opzione popolare e utile a causa della sua semplicità di implementazione.

## Per saperne di più
- Per una panoramica più completa dell'autenticazione di base: [HTTP Basic Access Documentation](https://datatracker.ietf.org/doc/html/rfc7617)
- Per prendere in considerazione metodi di autenticazione alternativi: [OAuth 2.0](https://oauth.net/2/)
- Per gli approfondimenti sulla libreria `Casablanca`: [C++ REST SDK Documentation](https://github.com/microsoft/cpprestsdk/blob/master/Release/README.md)