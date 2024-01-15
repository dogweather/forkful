---
title:                "Enviando uma solicitação HTTP"
html_title:           "C++: Enviando uma solicitação HTTP"
simple_title:         "Enviando uma solicitação HTTP"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Se você estiver trabalhando com comunicação em rede em um aplicativo C++, provavelmente precisará enviar uma solicitação HTTP para um servidor. Isso pode ser necessário para receber informações ou enviar dados, como formulários ou arquivos. Enviar uma solicitação HTTP é essencial para implementar funcionalidades como autenticação, troca de dados e integração com APIs de terceiros.

## Como Fazer

Para enviar uma solicitação HTTP em um aplicativo C++, você precisará usar a biblioteca padrão de C++, "libcurl", que oferece suporte ao protocolo HTTP e HTTPS. Abaixo está um exemplo de código que mostra como fazer uma solicitação GET simples usando o libcurl:

```C++
#include <curl/curl.h> // incluir a biblioteca libcurl

int main()
{
    CURL *curl; // criar um objeto CURL
    CURLcode res; // variável para armazenar o código de resposta

    curl = curl_easy_init(); // inicializar o objeto CURL

    if (curl) // verificar se a inicialização foi bem sucedida
    {
        // definir a URL que você deseja enviar a solicitação
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.exemplo.com/recurso");

        // estabelecer uma conexão
        res = curl_easy_perform(curl);

        // verificar se não há erros
        if (res != CURLE_OK) 
            fprintf(stderr, "Erro ao enviar a solicitação: %s\n", curl_easy_strerror(res));

        // limpar o objeto CURL
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

O código acima usa a função "curl_easy_perform" para executar a solicitação HTTP. No entanto, também é possível personalizar a solicitação, definindo opções adicionais, como o método HTTP, cabeçalhos, corpo da solicitação, entre outros.

## Deep Dive

Ao enviar uma solicitação HTTP, vários parâmetros são enviados em conjunto com a URL, como cabeçalhos (headers), corpo (body) e método. O método padrão é GET, que é usado para solicitar recursos do servidor. Existem outros métodos HTTP, como POST, PUT e DELETE, que são usados para criar, atualizar e excluir recursos, respectivamente.

O libcurl também oferece suporte a vários tipos de cabeçalhos, que podem ser definidos usando a função "curl_easy_setopt". Por exemplo, é possível definir o cabeçalho "Content-Type" para especificar o tipo de conteúdo que está sendo enviado no corpo da solicitação.

No código acima, o objeto "CURL" é responsável por armazenar as opções da solicitação e a função "curl_easy_setopt" é usada para definir essas opções. Além disso, a função "curl_easy_perform" é usada para enviar a solicitação e o código de resposta é armazenado na variável "res". O libcurl também possui funções para ler a resposta do servidor, se necessário.

## Veja também

- Documentação da biblioteca libcurl: https://curl.haxx.se/libcurl/
- Tutorial de libcurl em C++: https://codebeautify.org/curl-sample/0fa245
- Explicação detalhada sobre solicitações HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview