---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Fazendo requisições HTTP com Autenticação Básica usando Swift

## O Que & Porquê?
Enviar uma requisição HTTP com autenticação básica é como apresentar um crachá digital, provando quem você é para acessar determinado recurso online. Programadores fazem isso para acessar APIs protegidas e garantir que apenas usuários autenticados possam acessar esses dados.

## Como fazer:
Vamos realizar uma requisição HTTP usando o `URLSession` em Swift: 

```Swift
import Foundation

let username = "seu_nome"
let password = "sua_senha"
let loginString = "\(username):\(password)"

guard let loginData = loginString.data(using: String.Encoding.utf8) else { return }

let base64LoginString = loginData.base64EncodedString()

// Criação da requisição
var request = URLRequest(url: URL(string: "https://www.seuwebsite.com")!)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    // insira seu código aqui
}

task.resume()
```
Este código irá codificar suas credenciais de login em Base64 e incluí-las no cabeçalho da requisição HTTP como padrão da autenticação básica.

## Mergulho Profundo
A autenticação básica é uma das técnicas mais antigas utilizadas para autenticação em requisições HTTP. Surgiu como parte do padrão HTTP/1.0, porém com o advento de métodos mais seguros como o OAuth 2.0 e o JWT (Json Web Token), hoje, é menos utilizada.

Alternativas incluem, como mencionado, o OAuth, uma estrutura de autorização padrão que permite aplicações de terceiros acessarem dados do usuário sem precisar de suas credenciais.

No código acima, criamos uma `URLSession`. Uma implementação alternativa poderia usar bibliotecas de terceiros como `Alamofire`, que oferece uma interface mais amigável e limpa para gerenciar requisições HTTP.

## Veja Também
1. Documentação oficial do Swift sobre URLSession: https://developer.apple.com/documentation/foundation/urlsession.
2. Tutorial JWT em Swift: https://auth0.com/docs/quickstart/mobile/ios-swift.
3. Biblioteca Alamofire para Swift: https://github.com/Alamofire/Alamofire. 
4. Documentação do padrão OAuth 2.0: https://oauth.net/2/.