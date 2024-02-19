---
aliases:
- /pt/c/sending-an-http-request-with-basic-authentication/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:15.547222-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica em\
  \ C envolve criar uma solicita\xE7\xE3o HTTP que inclua um cabe\xE7alho de Autoriza\xE7\
  \xE3o com credenciais de\u2026"
lastmod: 2024-02-18 23:08:58.618923
model: gpt-4-0125-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica em C envolve\
  \ criar uma solicita\xE7\xE3o HTTP que inclua um cabe\xE7alho de Autoriza\xE7\xE3\
  o com credenciais de\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma requisição HTTP com autenticação básica em C envolve criar uma solicitação HTTP que inclua um cabeçalho de Autorização com credenciais de usuário codificadas em Base64. Este é um método comum para adicionar uma camada de autenticação simples às solicitações HTTP, permitindo que recursos restritos sejam acessados programaticamente.

## Como Fazer:
Para enviar uma requisição HTTP com autenticação básica em C, precisaremos usar a biblioteca libcurl, uma biblioteca de transferência de URL do lado do cliente popular, versátil e fácil de usar. Ela lida com vários protocolos, incluindo HTTP e HTTPS, simplificando nossa tarefa. Certifique-se de que o libcurl esteja instalado em seu sistema antes de prosseguir. Aqui está um exemplo básico que demonstra como enviar uma requisição GET com autenticação básica:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // A URL para qual a solicitação está sendo enviada
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Habilitando o uso de autenticação básica
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Fornecendo o nome de usuário e senha para a autenticação básica
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Executando a solicitação GET
        res = curl_easy_perform(curl);

        // Verificando se há erros
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() falhou: %s\n",
                    curl_easy_strerror(res));

        // Sempre limpe os recursos
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
No exemplo acima, substitua `"http://example.com/resource"`, `"username"` e `"password"` pela sua URL, nome de usuário e senha reais.

Este código inicializa um objeto `CURL`, define a URL, habilita a Autenticação Básica HTTP e especifica as credenciais. Em seguida, envia a solicitação e faz a limpeza após o uso. Se for bem-sucedido, o recurso solicitado é obtido; se houver um erro, ele é impresso no stderr.

A saída da amostra (assumindo autenticação bem-sucedida e acesso ao recurso) pode não ser mostrada diretamente pelo programa, já que o exemplo demonstra principalmente como enviar a solicitação. Para imprimir a resposta, você estenderia o programa para lidar com os dados da resposta HTTP.

## Aprofundamento:
Enviar solicitações HTTP com autenticação básica em C, conforme mostrado, aproveita a biblioteca libcurl pela sua robustez e simplicidade. Historicamente, criar solicitações HTTP puramente em C sem tais bibliotecas era trabalhoso e propenso a erros, envolvendo programação de soquete de baixo nível e construção manual de cabeçalhos HTTP.

A autenticação básica em si é um método dos primeiros dias da web. Ela envia credenciais em um formato facilmente decodificável (Base64), o que é inerentemente inseguro sobre canais de texto simples. Aplicações modernas frequentemente preferem métodos de autenticação mais seguros, como OAuth 2.0 ou JWT (JSON Web Tokens), especialmente para dados sensíveis.

No entanto, para sistemas internos menos críticos, ou scripts rápidos e práticos onde a conveniência supera as preocupações com segurança, a autenticação básica permanece em uso. Além disso, quando combinada com conexões criptografadas (HTTPS), sua simplicidade se torna uma vantagem para desenvolvimento rápido, teste ou trabalho de automação onde mecanismos de segurança de nível superior não são tão necessários.

Em contextos onde a segurança de ponta é inegociável, alternativas como a autenticação baseada em tokens devem ser priorizadas. No entanto, entender como implementar a autenticação básica em C através do libcurl fornece uma habilidade fundamental que pode ser adaptada para vários métodos de autenticação e protocolos, refletindo os compromissos nuanciados entre segurança, conveniência e requisitos de aplicação no desenvolvimento web.
