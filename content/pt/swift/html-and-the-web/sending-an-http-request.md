---
title:                "Enviando uma requisição HTTP"
date:                  2024-01-20T18:00:33.130433-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Enviar uma requisição HTTP é o processo de pedir dados ou enviar dados para um servidor web. Programadores fazem isso para interagir com APIs, para buscar informações ou enviar dados para serem processados e armazenados remotamente.

## How to:
No Swift, para enviar uma requisição HTTP, usamos principalmente a classe `URLSession`. Vou mostrar um exemplo rápido de como buscar um JSON de uma API:

```Swift
import Foundation

// Defina a URL da requisição
let url = URL(string: "https://api.exemplo.com/dados")!

// Crie a sessão e inicie a tarefa de requisição
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    // Verifique se houve erro
    guard let data = data, error == nil else {
        print("Erro na requisição: \(error?.localizedDescription ?? "Unknown error" )")
        return
    }
    // Tente decodificar os dados JSON
    do {
        if let jsonResult = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
            print("JSON recebido: \(jsonResult)")
        }
    } catch let error {
        print("Erro ao decodificar JSON: \(error.localizedDescription)")
    }
}

task.resume()
```

Executando esse código em um playground ou app iOS, você receberá a resposta (dependendo dos dados da API) como um dicionário de Swift que foi impresso no console.

## Deep Dive
Enviar requisições HTTP não é algo novo. Faz parte das bases da web desde seus primórdios. Mas as abordagens e ferramentas para isso no Swift evoluíram. Antigamente, usávamos `NSURLConnection`, mas ela foi substituída pela mais moderna `URLSession`.

Outras alternativas incluem bibliotecas de terceiros como Alamofire, que simplificam algumas tarefas mas adicionam dependências externas ao seu projeto.

Os detalhes de implementação importantes incluem a gestão de sessões de rede, tratamento de erros e parsing de dados. Uma consideração crítica é o tratamento de dados sensíveis e a configuração adequada da segurança da requisição, como a utilização de HTTPS.

## See Also
Para mais informações, confira:

- [A Documentação Oficial de URLSession da Apple](https://developer.apple.com/documentation/foundation/urlsession)
