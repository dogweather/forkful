---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviando Solicitações HTTP em Swift

## O Que & Por Quê?

Enviar uma solicitação HTTP é pedir a um servidor de internet que lhe forneça dados. Fazemos isso para acessar webservices, API's e outros recursos online.

## Como Fazer:

Para começar, vamos usar a biblioteca URLSession do Swift. O código abaixo envia uma solicitação GET para um URL especificado. Preste atenção nas funcionalidades Swift super convenientes para lidar com os erros.

```Swift
import Foundation

let url = URL(string: "http://www.example.com")!

let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    guard let data = data else { return }
    print(String(data: data, encoding: .utf8)!)
}

task.resume()
```

Quando você executar esse código, obterá a saída do HTML desta página.

## Um Mergulho Profundo

Quando falamos em solicitações HTTP, estamos falando sobre um protocolo baseado em texto que remonta a 1991! É surpreendente em parte porque ainda é relevante e amplamente em uso hoje.

Mas certamente existem alternativas. GraphQL é um deles, que oferece mais eficiência em muitos casos. Mesmo dentro das solicitações HTTP, podemos escolher entre GET, POST, DELETE e muito mais.

No que diz respeito aos detalhes de implementação no Swift, o URL, URLSession e URLRequest estão fazendo a maior parte do trabalho. O URL codifica o endereço do recurso, o URLRequest encapsula todos os detalhes do que estamos pedindo e o URLSession cuida do envio da solicitação.

## Veja Também

Para saber mais, você pode começar com a documentação oficial do Swift:

- URLSession: https://developer.apple.com/documentation/foundation/urlsession
- HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP

E se você estiver interessado em GraphQL como alternativa, dê uma olhada aqui:

- GraphQL: https://graphql.org/ 

Os padrões de design e os melhores métodos práticos mudam continuamente neste campo, por isso, sempre continue aprendendo!