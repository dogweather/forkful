---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "C++: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Você pode precisar enviar uma solicitação HTTP com autenticação básica para se comunicar com APIs ou servidores que requerem autenticação antes de permitir o acesso aos dados. Isso pode ser feito de forma segura e eficiente usando C++.

## Como Fazer

Para enviar uma solicitação HTTP com autenticação básica em C++, siga os seguintes passos:

1. Inclua a biblioteca "libcurl" em seu código, que permite a comunicação com servidores via protocolo HTTP.
2. Configure as opções da solicitação, como URL, método e cabeçalhos.
3. Defina a opção de autenticação básica usando a função `curl_easy_setopt()` e fornecendo seu nome de usuário e senha.
4. Execute a solicitação usando `curl_easy_perform()` e observe o código de status da resposta para garantir que foi bem-sucedida.

Um exemplo de código completo pode ser encontrado abaixo:

```C++
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    // Set URL and method
    curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/resource");
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");

    // Set basic authentication
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

    // Send request
    res = curl_easy_perform(curl);

    // Check for errors
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    // Clean up
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

A saída esperada seria uma resposta bem-sucedida com o código de status 200.

## Deep Dive

A autenticação básica é um método simples de autenticação em que o nome de usuário e a senha são passados em texto simples por meio do cabeçalho HTTP `Authorization`. É importante lembrar que essa forma de autenticação não é segura, pois a senha pode ser facilmente interceptada por alguém com acesso à rede.

Para uma camada adicional de segurança, é recomendado usar a autenticação via HTTPS, que criptografa a comunicação entre o cliente e o servidor.

## Veja Também

- Documentação oficial do libcurl: https://curl.se/libcurl/
- Tutorial sobre comunicação via HTTP com C++: https://www.aquantia.com/blog/2017/10/20/libcurl-tutorial/