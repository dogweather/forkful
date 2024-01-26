---
title:                "Enviando uma requisição HTTP com autenticação básica"
date:                  2024-01-20T18:01:01.392357-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Enviar uma requisição HTTP com autenticação básica é o método de incluir credenciais de usuário e senha em uma requisição HTTP. Programadores usam isso para acessar APIs ou recursos web que requerem identificação.

## Como Fazer:

```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://seuservidor.com/dados");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "usuario");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "senha");

        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "Erro na requisição: %s\n", curl_easy_strerror(res));
        }

        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Saída esperada (no console, se sucesso):

```
<Dados ou resposta do servidor>
```

## Mergulho Profundo:

O método de autenticação básica HTTP foi definido pela primeira vez em 1996, no RFC 1945, e mais tarde em RFC 2617. Hoje, a autenticação básica é considerada insegura para transmissão sobre redes não criptografadas porque as credenciais são codificadas com Base64, que é facilmente decodificável.

Alternativas mais seguras, como OAuth e autenticação de token, são frequentemente preferidas para novas aplicações. Contudo, a autenticação básica ainda é utilizada em contextos internos ou para testar rapidamente APIs que não exigem alta segurança.

Implementar um cliente de requisição com autenticação básica em C requer o uso de uma biblioteca externa como libcurl, uma biblioteca cliente de transferência de URL multiplataforma que suporta vários protocolos.

## Veja Também:

- Documentação `libcurl`: https://curl.se/libcurl/
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
- Tutorial sobre autenticação segura OAuth 2.0: https://oauth.net/2/
