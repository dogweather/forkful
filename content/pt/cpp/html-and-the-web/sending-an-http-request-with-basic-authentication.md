---
date: 2024-01-20 18:01:31.866743-07:00
description: "How to: (Como Fazer:) Vamos ver como fazer isso em C++. Primeiro, voc\xEA\
  \ precisa de uma biblioteca de rede, como a cURL. Instale-a se necess\xE1rio e aqui\
  \ est\xE1\u2026"
lastmod: '2024-04-05T21:53:47.230528-06:00'
model: gpt-4-1106-preview
summary: (Como Fazer:) Vamos ver como fazer isso em C++.
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

## How to: (Como Fazer:)
Vamos ver como fazer isso em C++. Primeiro, você precisa de uma biblioteca de rede, como a cURL. Instale-a se necessário e aqui está um exemplo de código:

```c++
#include <iostream>
#include <string>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;
    std::string userPwd = "usuario:senha"; // Substitua com suas credenciais

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://seu-servidor.com/recurso"); // Substitua com a URL
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPwd.c_str());
        
        res = curl_easy_perform(curl);
        
        if(res != CURLE_OK)
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
    return 0;
}
```

Compilando e executando esse programa, a resposta do servidor será impressa diretamente no console.

## Deep Dive (Mergulho Profundo)
Historicamente, a autenticação básica via HTTP surgiu como um método simples para controlar acesso a recursos na web. Envolve codificar em base64 as credenciais do usuário e incluí-las no cabeçalho da requisição.

Existem alternativas mais seguras, como OAuth e autenticação de token JWT, que são recomendadas para produção devido a maiores garantias de segurança.

Na implementação com a biblioteca cURL, `CURLOPT_HTTPAUTH` e `CURLOPT_USERPWD` são opções que definem o tipo de autenticação e as credenciais, respectivamente. Lembre-se de que a transmissão de credenciais sem uma camada de segurança, como HTTPS, expõe a riscos de interceptação.

## See Also (Veja Também)
- cURL library: https://curl.se/libcurl/
- HTTP basic authentication standards: https://tools.ietf.org/html/rfc7617
- Base64 encoding: https://en.wikipedia.org/wiki/Base64
