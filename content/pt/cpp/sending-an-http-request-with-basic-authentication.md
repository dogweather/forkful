---
title:                "C++: Enviando uma solicitação HTTP com autenticação básica"
simple_title:         "Enviando uma solicitação HTTP com autenticação básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Há várias razões pelas quais alguém pode querer enviar uma solicitação HTTP com autenticação básica. Uma das principais é para garantir a segurança dos dados transmitidos entre o cliente e o servidor. Ao adicionar uma autenticação básica à solicitação, o servidor pode verificar se o usuário tem permissão para acessar os dados solicitados antes de enviá-los.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em C++, primeiro precisamos incluir a biblioteca `curl` e as bibliotecas de string e stream padrão do C++. Em seguida, criamos um objeto `curl`, definimos a URL alvo e adicionamos as credenciais de autenticação básica ao cabeçalho da solicitação. Aqui está um exemplo de código:

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

int main()
{
    // Cria um objeto CURL
    CURL *curl = curl_easy_init();
    if (!curl)
    {
        std::cout << "Erro ao inicializar o CURL!" << std::endl;
        return 1;
    }

    // Define a URL alvo
    std::string url = "https://exemplo.com/api/dados";

    // Adiciona as credenciais de autenticação básica ao cabeçalho da solicitação
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "usuario:senha"); // Substitua com suas próprias credenciais
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());

    // Realiza a solicitação HTTP
    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK)
    {
        std::cout << "Erro ao realizar a solicitação: " << curl_easy_strerror(res) << std::endl;
        return 1;
    }

    // Limpa o objeto CURL
    curl_easy_cleanup(curl);

    return 0;
}
```

O código acima realizará uma solicitação GET para a URL especificada, adicionando as credenciais de autenticação básica ao cabeçalho da solicitação.

O resultado da solicitação será mostrado no console, mas também é possível realizar outras ações com os dados recebidos, como salvá-los em um arquivo ou manipulá-los em uma estrutura de dados.

## Deep Dive

Quando enviamos uma solicitação HTTP com autenticação básica, o cabeçalho da solicitação será parecido com o seguinte:

```
GET /api/dados HTTP/1.1
Host: exemplo.com
Authorization: Basic dXN1YXJpbzpzZW5oYTEyMw==
```

Nesse caso, as credenciais de autenticação básica são codificadas no formato Base64, o que significa que elas são facilmente decodificadas. Portanto, não é recomendado usar a autenticação básica como a única medida de segurança para suas solicitações HTTP.

Em vez disso, é recomendado utilizar outros métodos de autenticação mais seguros, como a autenticação com tokens ou certificados.

## Veja também

- [Documentação oficial do CURL](https://curl.se/docs/)
- [Tutorial de autenticação básica com CURL](https://www.codegrepper.com/code-examples/cpp/c%2B%2B+curl+basic+authentication)