---
title:                "Enviando uma requisição http com autenticação básica"
html_title:           "Swift: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?

Enviar uma solicitação HTTP com autenticação básica é uma forma de garantir que o acesso a certas informações ou recursos esteja restrito apenas àqueles que possuem as credenciais corretas. Programadores utilizam esse método para proteger suas APIs e serviços web de acessos não autorizados.

## Como fazer:

``` Swift
// Crie uma instância da URL com a URL da API
let url = URL(string: "https://exemplo.com/api")!

// Crie uma solicitação HTTP com autenticação básica
var request = URLRequest(url: url)
request.httpMethod = "GET"

// Crie as credenciais de acesso
let username = "usuário"
let password = "senha"
let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

// Adicione o cabeçalho de autenticação à solicitação
let authString = "Basic \(base64LoginString)"
request.addValue(authString, forHTTPHeaderField: "Authorization")

// Inicie a sessão de URL e envie a solicitação
let session = URLSession.shared
let task = session.dataTask(with: request) { data, response, error in
    // Manipule a resposta ou o erro aqui
}
task.resume()
```

## Detalhes adicionais:

Autenticação básica é um método de autenticação que foi especificado na RFC 2617. É amplamente utilizado na web e é um dos métodos mais simples e fáceis de implementar. Outras alternativas incluem OAuth e JWT, que oferecem mais recursos e segurança.

Ao enviar uma solicitação HTTP com autenticação básica, as credenciais de login são codificadas em string e adicionadas ao cabeçalho da solicitação. É importante notar que, apesar do nome "básico", esse método não é recomendado para uso em ambientes de produção, pois as credenciais são enviadas em texto simples e podem ser facilmente interceptadas. Portanto, é sempre recomendado utilizar protocolos de segurança mais avançados para proteger APIs e serviços web.

## Veja também:

- [RFC 2617](https://tools.ietf.org/html/rfc2617)
- [Autenticação basic in Swift](https://developer.apple.com/documentation/foundation/url_loading_system/additional_authentication_schemes/basic_authentication_in_swift)