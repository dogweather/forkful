---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "C: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Porquê

Você pode se perguntar por que alguém se preocuparia em enviar uma solicitação HTTP com autenticação básica. A resposta é simples: autenticação básica é um dos métodos mais simples e amplamente suportados para proteger o acesso a recursos na web.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em C, você precisará usar a biblioteca `libcurl`. Aqui está um exemplo de código que mostra como fazer isso:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;

  // Inicializa o libcurl
  curl = curl_easy_init();
  if(curl) {
    // Define a URL de destino
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.exemplo.com");

    // Define a autenticação básica
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

    // Envia a solicitação HTTP GET
    res = curl_easy_perform(curl);

    // Verifica o resultado da solicitação
    if(res != CURLE_OK)
      fprintf(stderr, "Falha na solicitação: %s\n", curl_easy_strerror(res));

    // Encerra o libcurl
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Ao executar este código, você verá a saída mostrando o conteúdo da página solicitada. Note que é importante fornecer o nome de usuário e senha corretos para que a autenticação básica funcione corretamente.

## Mergulho Profundo

A autenticação básica funciona enviando o nome de usuário e senha como parte dos cabeçalhos da solicitação HTTP. Isso significa que, se alguém interceptar a solicitação, eles terão acesso às suas credenciais. Portanto, é importante que você só use autenticação básica em conexões seguras (HTTPS), ou em casos em que a segurança não é uma preocupação.

Veja também:

- [Documentação do libcurl](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html)
- [Guia de autenticação HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)