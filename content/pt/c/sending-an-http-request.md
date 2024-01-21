---
title:                "Enviando uma requisição HTTP"
date:                  2024-01-20T17:59:26.779875-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Enviar uma solicitação HTTP permite que o seu programa converse com a web. Programadores fazem isso para buscar dados, interagir com serviços ou APIs e trocar informações entre diferentes sistemas.

## Como Fazer:

Para enviar uma solicitação HTTP em C, você pode usar a libcurl, uma biblioteca versátil que lida com URLs. Vamos lá:

```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        // Perform the request, res will get the return code 
        res = curl_easy_perform(curl);
        
        // Check for errors 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        
        // Cleanup
        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
    return 0;
}
```

Esse código simples faz uma solicitação GET para "http://example.com".

## Mergulho Profundo:

A libcurl tem uma história rica, iniciada em 1997, e evoluiu para suportar uma vasta gama de protocolos além do HTTP. Alternativas como sockets POSIX podem ser usadas, mas exigem mais código e conhecimento detalhado dos protocolos. Com a libcurl, o manuseio dos protocolos é abstraído, permitindo focar na lógica do seu programa. Na implementação, é crucial liberar recursos e evitar vazamentos de memória, como fazemos com `curl_easy_cleanup()` após a conclusão da solicitação.

## Veja Também:

- Documentação oficial da libcurl: https://curl.se/libcurl/c/
- HTTP para principiantes: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Basics_of_HTTP
- Sockets em C (Uma Alternativa Avançada): https://beej.us/guide/bgnet/