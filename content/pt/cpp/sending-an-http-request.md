---
title:                "Enviando uma solicitação http"
html_title:           "C++: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Enviar uma solicitação HTTP é o ato de enviar uma mensagem para um servidor web para solicitar um recurso específico. Os programadores fazem isso para interagir com aplicativos e obter dados ou executar ações em um servidor.

## Como Fazer:

```C++
#include <iostream>
#include <curl/curl.h> // biblioteca para fazer solicitações HTTP

// função para callback utilizado pela biblioteca curl
static int writer(char *data, size_t size, size_t nmemb, std::string *buffer)
{
    int result = 0;
    if (buffer != NULL) {
        buffer->append(data, size * nmemb);
        result = size * nmemb;
    }
    return result;
}

int main(void)
{
  CURL *curl;
  CURLcode res;
  std::string buffer; // variável para armazenar os dados recebidos

  curl = curl_easy_init();
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.google.com/"); // endereço da solicitação
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer); // define a função de callback
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer); // passa o buffer para armazenar os dados
    res = curl_easy_perform(curl);

    // verifica se a solicitação foi bem-sucedida
    if (res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl); // libera a memória alocada
  }

  std::cout << buffer << std::endl; // imprime os dados recebidos
  return 0;
}

```

O código acima utiliza a biblioteca cURL, uma opção popular para manipular solicitações HTTP em C++. Primeiro, é criada uma função de callback para ser chamada pela biblioteca curl. Em seguida, é iniciada uma sessão curl e definidos a URL da solicitação, a função de callback e o buffer para armazenar os dados recebidos. Por fim, é realizada a solicitação e os dados são impressos.

## Detalhes:

Existem várias bibliotecas e ferramentas disponíveis para enviar solicitações HTTP em C++. Além do cURL, outras opções populares incluem o Poco C++ Libraries e o cpp-httplib.

## Veja Também:

- [Tutorial cURL](https://curl.haxx.se/libcurl/c/simple.html)
- [Página oficial do Poco C++ Libraries](https://pocoproject.org/)
- [Repositório do cpp-httplib no GitHub](https://github.com/yhirose/cpp-httplib)