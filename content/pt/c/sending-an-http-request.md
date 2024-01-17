---
title:                "Enviando uma requisição http"
html_title:           "C: Enviando uma requisição http"
simple_title:         "Enviando uma requisição http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Enviar uma solicitação HTTP é um processo importante na programação, pois permite que os desenvolvedores interajam com servidores e acessem informações da web. Essa solicitação pode ser feita por meio de um navegador da web ou por meio de um programa de computador.

## Como fazer:

Um exemplo simples de envio de uma solicitação GET pode ser feito da seguinte forma em C:
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://exemplo.com"); // URL do servidor
    res = curl_easy_perform(curl); // solicitação GET
    if(res != CURLE_OK)
      fprintf(stderr, "Erro ao enviar solicitação: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl); // limpar CURL
  }
  return 0;
}
```

O resultado dessa solicitação seria um código HTML da página do servidor, que pode ser usado para extrair informações específicas ou tomar ações.

## Mergulho profundo:

O protocolo HTTP (Hypertext Transfer Protocol) é a base de comunicação da web. Foi desenvolvido no início da década de 1990 e é amplamente utilizado até hoje. Existem outros protocolos que também podem ser usados para enviar solicitações, como HTTPS e FTP.

Além do libcurl, existem outras bibliotecas que podem ser utilizadas para enviar solicitações HTTP em C, como o HTTPClient e a biblioteca nativa do Windows, WinINet. Além disso, muitas linguagens de programação possuem recursos embutidos para enviar solicitações HTTP, como a biblioteca Requests em Python.

A implementação exata de uma solicitação HTTP pode variar dependendo da especificação do protocolo e do servidor em questão. É importante estar ciente das diferentes opções e parâmetros disponíveis ao enviar uma solicitação.

## Veja também:

- [Documentação libcurl](https://curl.haxx.se/libcurl/)
- [Documentação HTTPClient](https://developer.apple.com/documentation/httpclient)
- [Documentação WinINet](https://docs.microsoft.com/en-us/windows/win32/wininet/wininet?redirectedfrom=MSDN)
- [Documentação Requests](https://docs.python-requests.org/en/master/)