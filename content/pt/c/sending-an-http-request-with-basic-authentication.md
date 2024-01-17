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

## O que & Por quê?

Enviar uma solicitação HTTP com autenticação básica é um processo que permite aos programadores acessarem recursos protegidos em um servidor. Basicamente, a autenticação básica envia as credenciais do usuário (nome de usuário e senha) em formato codificado em base64 no cabeçalho da solicitação HTTP. Isso é feito para garantir que apenas usuários autorizados tenham acesso aos recursos protegidos.

## Como fazer:

```C
#include <stdio.h>
#include <curl/curl.h> // Biblioteca para realizar solicitações HTTP

int main(void)
{
    // Inicializa a biblioteca cURL
    curl_global_init(CURL_GLOBAL_ALL);

    // Cria um identificador para a sessão cURL
    CURL *curl = curl_easy_init();

    if (curl)
    {
        // Define a URL de destino
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.exemplo.com/api/recurso");
        // Define a opção para solicitar com autenticação básica
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        // Define as credenciais de usuário e senha
        curl_easy_setopt(curl, CURLOPT_USERNAME, "usuario");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "senha");

        // Executa a solicitação HTTP
        CURLcode res = curl_easy_perform(curl);

        // Se a solicitação for bem-sucedida
        if (res == CURLE_OK)
        {
            // Armazena o código de resposta HTTP
            long response_code;
            curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
            printf("Código de resposta HTTP: %ld\n", response_code);
        }
        else
        {
            printf("Erro ao enviar solicitação: %s\n", curl_easy_strerror(res));
        }

        // Libera o identificador cURL
        curl_easy_cleanup(curl);
    }

    // Finaliza a biblioteca cURL
    curl_global_cleanup();

    return 0;
}
```

Exemplo de saída:

```
Código de resposta HTTP: 200
```

## Mergulhando fundo:

A autenticação básica foi introduzida em 1999 como parte do protocolo HTTP. Desde então, outras formas de autenticação, como a autenticação por token, se tornaram mais populares devido à segurança aprimorada. No entanto, a autenticação básica ainda é amplamente utilizada em aplicações legadas que não suportam outras formas de autenticação. Além disso, deve-se ter cuidado ao usar a autenticação básica, pois as credenciais são transmitidas em texto simples, o que pode ser um risco de segurança em certas situações.

É possível realizar solicitações HTTP com autenticação básica usando outras bibliotecas, como libcurl em C++, HTTPClient em Java ou Requests em Python. Além disso, é importante lembrar que o servidor deve suportar a autenticação básica e o formato do cabeçalho da solicitação deve estar de acordo com o padrão especificado pelo protocolo HTTP.

## Veja também:

[Documentação da API cURL](https://curl.se/libcurl/c/CURLOPT_HTTPAUTH.html)

[Tutorial sobre autenticação básica com cURL em C](https://www.geeksforgeeks.org/send-post-request-using-curl-library-in-c/)