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

## O que é e porquê?

Enviar uma HTTP request é simplesmente uma forma de enviar uma solicitação para um servidor através da internet. Programadores fazem isso o tempo todo para obter dados de um servidor ou para enviar informações para um servidor, como em uma aplicação móvel conectada à internet.

## Como fazer:

```Swift
import Foundation

// Configurando a URL a ser solicitada
let url = URL(string: "https://www.example.com/api")!

// Criando uma HTTP request com o verbo GET
var request = URLRequest(url: url)
request.httpMethod = "GET"

// Realizando a solicitação
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    // Tratando os dados recebidos
    if let data = data {
        print(String(data: data, encoding: .utf8)!)
    }
}
task.resume()
```

## Mergulho a fundo:

Enviar HTTP requests já é uma prática antiga na programação, utilizado principalmente em aplicações web. Hoje em dia, com o avanço das tecnologias móveis e a popularidade de APIs, a comunicação através de solicitações HTTP também é fundamental em aplicações móveis.

Existem algumas alternativas para enviar HTTP requests, como o uso de bibliotecas externas ou o uso de frameworks de rede, mas a forma nativa em Swift é bastante eficiente. É importante se atentar às boas práticas de segurança e sempre tratar os dados recebidos de forma correta.

## Veja também:

- [Documentação oficial da Apple sobre enviar HTTP requests em Swift](https://developer.apple.com/documentation/foundation/url_loading_system)
- [Artigo da IBM sobre comunicação entre aplicativos iOS e servidores através de solicitações HTTP](https://www.ibm.com/developerworks/library/ios-apps-iosweb/)
- [Tutorial em vídeo sobre como enviar HTTP requests em Swift](https://www.youtube.com/watch?v=uprQbrYefkI)