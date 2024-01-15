---
title:                "Enviando uma solicitação http"
html_title:           "Swift: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Enviar uma solicitação HTTP é uma forma essencial de comunicação entre dispositivos e servidores da web. Ao enviar uma solicitação HTTP, o dispositivo pode obter informações, atualizações ou interagir com um servidor da web.

## Como Fazer

```Swift
import UIKit

func sendHTTPRequest(url: String) {
    let request = URLRequest(url: URL(string: url)!)
    let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
        guard let response = response as? HTTPURLResponse, let data = data else {
            print("Erro ao enviar solicitação HTTP")
            return
        }

        if response.statusCode == 200 {
            // sucesso
            print(data)
        } else {
            // erro
            print("Erro ao obter dados. Código de erro: \(response.statusCode)")
        }
    }
    task.resume()
}

// exemplo de solicitação HTTP
let url = "https://www.example.com/api/getData"
sendHTTPRequest(url: url)
```

O código acima mostra uma função simples para enviar uma solicitação HTTP usando a classe `URLSession` da API `Foundation`. Primeiro, criamos uma `URLRequest` com o URL desejado. Em seguida, usamos essa `URLRequest` para criar uma `URLSessionDataTask` que executa a solicitação e retorna uma resposta HTTP, que podemos verificar para determinar o sucesso ou erro da solicitação. Se a resposta tiver um código de status 200 (OK), podemos acessar os dados retornados na resposta.

## Mergulho Profundo

Uma solicitação HTTP é composta por um número de elementos, como o URL do servidor, o método usado (GET, POST, etc.), cabeçalhos e corpo da solicitação. Além disso, podemos especificar parâmetros ou dados para serem enviados junto com a solicitação. Existem também diferentes tipos de solicitações HTTP, como `GET`, `POST`, `PUT` e `DELETE`, que são usadas para diferentes propósitos.

Ao enviar uma solicitação HTTP, também é importante considerar a segurança, pois informações confidenciais podem ser enviadas pelo corpo da solicitação. Portanto, é altamente recomendável usar protocolos de criptografia, como HTTPS, ao enviar solicitações HTTP.

## Veja Também

- [Apple Developer Documentation - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Retrieving Data From the Web - Hacking with Swift](https://www.hackingwithswift.com/example-code/networking/how-to-download-json-from-a-url-coding-challenge)
- [HTTP Requests in Swift - Medium](https://medium.com/@tomgielen/http-requests-in-swift-c6e78b82a89d)