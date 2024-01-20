---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando uma Solicitação HTTP com Autenticação Básica em C++

## O Que & Por Quê?
Enviar uma solicitação HTTP com autenticação básica em C++ refere-se ao processo de solicitar recursos de um servidor web utilizando um nome de usuário e uma senha. Programadores geralmente fazem isso para acessar APIs ou dados protegidos por senha em servidores web.

## Como fazer:
Aqui está um exemplo simples de como enviar uma solicitação HTTP GET com autenticação básica em C++ usando a biblioteca `cpp-httplib`.

```C++
#include "httplib.h"

int main() {
    httplib::Client cli("httpbin.org");
    httplib::Headers headers = {
        { "Authorization", "Basic " + httplib::detail::base64_encode("user:pass") }
    };

    if (auto res = cli.Get("/basic-auth/user/pass", headers)) {
        if (res->status == 200) {
            std::cout << res->body << std::endl;
        } else {
            std::cout << "Erro: " << res->status << std::endl;
    }} else {
        auto err = res.error();
        std::cout << "Erro no Envio: " << err << std::endl;
    }

    return 0;
}
```

Este script fará uma solicitação GET para `"https://httpbin.org/basic-auth/user/pass"` com um cabeçalho de autorização básica.

## Visão Mais Profunda
1. **Contexto histórico**: A autenticação básica HTTP é um método de autenticação que foi proposto pela primeira vez em 1996 como parte do padrão HTTP/1.0. Apesar de sua idade, ainda é amplamente utilizada devido à sua simplicidade.
2. **Alternativas**: Existem muitos outros métodos de autenticação HTTP, como Digest, Token, OAuth e JWT. Cada um tem seus próprios usos e benefícios, e sua escolha depende das necessidades do seu projeto.
3. **Detalhes de implementação**: `cpp-httplib` é uma biblioteca de rede HTTP/HTTPS síncrona em C++. Esta biblioteca suporta a autenticação básica ao adicionar o cabeçalho de autorização.

## Veja Também
- Documentação `cpp-httplib`: https://github.com/yhirose/cpp-httplib
- RFC2617 (HTTP Authentication: Basic and Digest Access Authentication): https://tools.ietf.org/html/rfc2617
- Solicitação HTTP GET em C++: https://stackoverflow.com/questions/1011339/how-do-you-make-a-http-request-with-c