---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar um pedido HTTP é um modo de um programa se comunicar com outros sistemas ou servidores na internet. Programadores fazem isso para trocar informações com APIs web, obter conteúdo de páginas da web e para diferentes formas de interação entre sistemas.

## Como fazer:

Podemos usar a biblioteca cURL em C++ para enviar solicitações HTTP. Aqui está um exemplo de código que envia um GET request.

```C++
#include <curl/curl.h>
#include <iostream>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        CURLcode res;
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            std::cerr << curl_easy_strerror(res) << std::endl;
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Neste código, primeiro inicializamos uma sessão cURL e especificamos a URL para a qual queremos enviar o request. Em seguida, chamamos `curl_easy_perform` para enviar o request, verificamos se ocorreu algum erro e, finalmente, limpamos para liberar recursos.

## Mergulho profundo 

Historicamente, enviar solicitações HTTP em C++ era bastante problemático, pois a linguagem não fornece recursos para isso por padrão. A biblioteca cURL, desenvolvida em 1997, simplificou muito esse processo.

Como alternativa ao cURL, temos a biblioteca Boost.Asio que fornece capacidades de rede assíncrona e pode ser usada para enviar solicitações HTTP. Além disso, o C++20 (a versão mais recente) está planejando incluir suporte para as operações de rede no STL (Standard Template Library), que pode tornar o cURL e bibliotecas similares obsoletas.

Um pedido HTTP envolve a construção de um texto no formato específico, enviado para um servidor da web usando o protocolo TCP/IP. A resposta, fornecida pelo servidor, também é um texto que deve ser analisado para extrair as informações desejadas.

## Veja também

1. Documentação cURL: https://curl.haxx.se/libcurl/c/
2. Tutorial Boost.Asio: https://think-async.com/Asio/asio-1.18.1/doc/asio/tutorial.html
3. Proposta de rede para C++20: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p1100r0.pdf
4. Tutorial de pedidos HTTP: https://www.w3schools.com/tags/ref_httpmethods.asp