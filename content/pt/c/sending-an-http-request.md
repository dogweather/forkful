---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviando uma solicitação HTTP em C

## O que & Por quê?
Enviar uma solicitação HTTP é o ato de pedir a um servidor web, através de HTTP, por dados ou ações para serem executadas. Os programadores o fazem para se comunicar, interagir e operar na internet.

## Como Fazer:
Aqui está um exemplo de como enviar uma solicitação HTTP usando a biblioteca `curl` em C.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    res = curl_easy_perform(curl);
    /* verifica o erro */
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() falhou: %s\n",
              curl_easy_strerror(res));
    /* sempre limpe */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Quando isso é executado, uma solicitação HTTP GET é enviada para `http://example.com`.

## Mergulho Profundo
Enviar uma solicitação HTTP é um dos aspectos básicos do trabalho na web e existem muitas maneiras de fazê-lo. O exemplo usou `libcurl`, escolhido pela sua facilidade de uso e apoio da comunidade. 

HTTP foi desenvolvido por Tim Berners-Lee e seu time no CERN no começo dos anos 90. Tem sido o alicerce da World Wide Web desde então, com várias versões lançadas ao longo dos anos.

Existem várias outras bibliotecas em C para enviar solicitações HTTP, como `libwww-c`, `neon`, `serf` entre outros. Você pode escolher uma dependendo das necessidades do seu projeto.

A implementação da solicitação HTTP envolve a criação de um socket, configuração do cabeçalho HTTP e envio dos dados de solicitação. A resposta do servidor é então recebida e processada.

## Veja Também
- Documentação Libcurl: [https://curl.haxx.se/libcurl/c/](https://curl.haxx.se/libcurl/c/)
- Tutorial Libcurl: [https://curl.se/libcurl/c/libcurl-tutorial.html](https://curl.se/libcurl/c/libcurl-tutorial.html)
- Detalhes do HTTP: [https://developer.mozilla.org/p/http](https://developer.mozilla.org/p/http)