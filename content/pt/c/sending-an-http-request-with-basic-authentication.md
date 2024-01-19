---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Enviar um HTTP request com autenticação básica é um procedimento comum em programação. Trata-se de um processo que requer que um cliente forneça um nome de usuário e senha para acessar arquivos específicos em um servidor. Os programadores fazem isso para proteger os arquivos e individualizar o acesso de acordo com as credenciais do cliente.

## Como fazer:

Aqui é um exemplo de como se pode conseguir isso através do uso de uma biblioteca de Carnaval, que simplifica a criação de pedidos HTTP.
```C
#include <curl/curl.h>

int main(void){
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:8000/");

    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "user:password");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```
## Mergulho profundo:

Envia um HTTP request com autenticação básica é um processo que existe desde o desenvolvimento inicial da web, projetado para proteger o conteúdo sensível. Como uma solução alternativa, é possível usar tokens em vez de autenticação básica, que são considerados mais seguros, mas também mais complexos de implementar. 

Ao enviar um pedido através do método acima, um cabeçalho de autorização é adicionado ao HTTP header com as credenciais de acesso do usuário, codificadas em Base64. Vale lembrar que a autenticação básica é simples, mas não oferece muita segurança. As credenciais são apenas codificadas, não criptografadas, tornando-as inseguras para transmissão em redes não confiáveis.

## Veja também:

- Tutorial completo da biblioteca [cURL](https://curl.haxx.se/)
- Documentação sobre [base64](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Headers/Authorization)
- Alternativas à autenticação básica, como [Opaque Tokens](https://oauth.net/2/token-introspection/) ou [JWT](https://jwt.io/introduction/).