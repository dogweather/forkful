---
title:                "Baixando uma página da web"
html_title:           "C: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Baixar uma página da web é o ato de transferir seu conteúdo para o seu dispositivo de computação. Programadores o fazem para acessar informações ou recursos úteis que estão contidos na página.

## Como Fazer:

~~~ C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  return 0;
}
~~~

O código acima utiliza a biblioteca Curl para realizar o download da página https://www.example.com/. A função `curl_easy_perform()` executa a transferência e a função `curl_easy_cleanup()` libera a memória alocada para a operação.

## Mergulho Profundo:

O ato de baixar páginas da web tem sido uma tarefa essencial para os programadores desde os primórdios da internet. Hoje, há outras formas de acessar o conteúdo de uma página, como a API de programação JavaScript `fetch()`, porém ainda existem casos em que o download direto é necessário. A função `curl_easy_setopt()` possui diversas opções de configuração, como definir parâmetros de autenticação ou incluir cabeçalhos HTTP personalizados.

## Veja Também:

- [Documentação da biblioteca Curl](https://curl.se/libcurl/c/)
- [Página oficial do Curl](https://curl.se/)
- [Exemplos de uso do Curl](https://curl.se/examples.html)