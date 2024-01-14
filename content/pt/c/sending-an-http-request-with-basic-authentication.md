---
title:                "C: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica

Enviar uma solicitação HTTP com autenticação básica é uma maneira segura de se comunicar com um servidor. Isso é especialmente útil quando se trabalha com dados confidenciais ou acessando informações restritas. A autenticação básica garante que apenas usuários autorizados tenham acesso aos dados necessários.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em C, é necessário seguir alguns passos simples. Primeiro, é preciso incluir a biblioteca padrão "curl.h". Em seguida, definimos a URL que será acessada e criamos uma variável com as informações de autenticação necessárias. Depois, usamos a função "curl_easy_setopt()" para configurar a solicitação com as opções corretas, incluindo as credenciais de autenticação. Por fim, é só realizar a chamada "curl_easy_perform()" para enviar a solicitação e receber a resposta do servidor.

Um exemplo de código completo para enviar uma solicitação HTTP com autenticação básica pode ser visto abaixo:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  // Definir URL e variáveis de autenticação
  char* url = "https://exemplo.com";
  char* username = "usuário";
  char* password = "senha";
  
  // Inicializar libcurl
  CURL *curl;
  curl = curl_easy_init();
  
  // Configurar a URL e as opções de autenticação
  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
  curl_easy_setopt(curl, CURLOPT_USERPWD, username+password);
  
  // Enviar a solicitação e receber a resposta
  CURLcode response = curl_easy_perform(curl);
  
  // Verificar se a solicitação foi bem sucedida
  if (response == CURLE_OK)
    printf("Solicitação enviada com sucesso!");
  else
    printf("Houve um erro ao enviar a solicitação: %s", curl_easy_strerror(response));
    
  // Finalizar a libcurl e liberar memória
  curl_easy_cleanup(curl);
  return 0;
}
```

O output deste código vai depender da resposta do servidor. Pode ser uma mensagem de sucesso ou um código de erro caso a solicitação não tenha sido bem sucedida.

## Mergulho aprofundado

A autenticação básica em uma solicitação HTTP funciona através da adição de um header chamado "Authorization" com as credenciais do usuário em formato codificado. Isso garante que as informações de autenticação não sejam transmitidas em texto claro, tornando a comunicação mais segura.

Vale ressaltar que a autenticação básica é considerada uma forma mais fraca de autenticação em comparação com outras opções, como a autenticação por chaves digitais. Por isso, é importante considerar outras formas de segurança, especialmente quando se está manipulando informações sensíveis.

## Veja também

- [Documentação oficial da libcurl](https://curl.se/libcurl/)
- [Tutorial completo de autenticação básica em C com libcurl](https://www.codegrepper.com/code-examples/c/curl+basic+authentication)
- [Explicação sobre diferentes tipos de autenticação em uma solicitação HTTP](https://www.ionos.com/digitalguide/websites/web-development/basic-and-digest-access-authentication-for-a-http-request/)
- [Exemplo de uso de autenticação básica em uma aplicação web com Node.js](https://dev.to/ummahusla/introduction-to-basic-authentication-with-node-js-5lh)