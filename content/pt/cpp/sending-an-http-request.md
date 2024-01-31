---
title:                "Enviando uma requisição HTTP"
date:                  2024-01-20T17:59:20.927289-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Enviar uma requisição HTTP é o processo de pedir dados ou recursos de um servidor usando o protocolo HTTP. Programadores fazem isso para interagir com APIs, acessar conteúdos web, ou comunicar entre diferentes sistemas.

## Como Fazer:
Vamos usar a biblioteca `cpr` que é uma abstração da `libcurl` em C++ para facilitar o envio de requisições HTTP.

Primeiro, instale a biblioteca:

```sh
$ sudo apt-get install libcurl4-openssl-dev
$ git clone https://github.com/libcpr/cpr.git
$ cd cpr
$ mkdir build && cd build
$ cmake ..
$ make
$ sudo make install
```

Agora, escreva o código:

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url("http://httpbin.org/get"));

    std::cout << "Status code: " << r.status_code << std::endl; // Exemplo: 200
    std::cout << "Response: " << r.text << std::endl;           // Resposta do servidor em texto
    
    return 0;
}
```

Exemplo de saída:

```
Status code: 200
Response: {
  "args": {}, 
  "headers": {
    ...
  }, 
  "origin": "24.127.96.129", 
  "url": "https://httpbin.org/get"
}
```

## Aprofundamento
Enviar requisições HTTP é fundamental desde que a internet se tornou povoada por APIs e serviços baseados em web. Antes de bibliotecas como `cpr`, a `libcurl` era comumente usada em C++, mas ela é complexa e verbosa. Alternativas modernas como `cpr` abstraem esses detalhes, mantendo o código limpo e mais mantível.

A `libcurl` é uma das mais poderosas e versáteis bibliotecas para transferências de URL disponíveis, suportando uma vasta gama de protocolos. Por outro lado, `cpr` fornece uma interface moderna e simplificada para tarefas comuns de HTTP(S). Vale notar que ao operar em nível tão baixo como o HTTP, detalhes como cabeçalhos (headers), métodos de requisição (GET, POST, etc), e estados de resposta (códigos de status) se tornam críticos para o bom funcionamento do aplicativo.

## Veja Também
- Documentação da biblioteca `cpr`: https://whoshuu.github.io/cpr/
- `libcurl` tutorial oficial: https://curl.se/libcurl/c/libcurl-tutorial.html
- HTTPbin para testar requisições HTTP: http://httpbin.org/
- Documentação da API HTTP do Mozilla Developer Network (MDN): https://developer.mozilla.org/en-US/docs/Web/HTTP
