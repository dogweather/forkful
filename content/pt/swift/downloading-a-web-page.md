---
title:                "Baixando uma página da web"
html_title:           "Swift: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que?

Você já se perguntou como obter uma página da web diretamente para o seu aplicativo? Baixar uma página da web permite que você acesse seu conteúdo e utilize-o de maneira dinâmica em seu aplicativo. Isso pode ser útil para exibir notícias, previsão do tempo ou qualquer outra informação online dentro do seu aplicativo.

## Como fazer?

Para baixar uma página da web em Swift, você precisará seguir alguns passos simples:

1. Primeiro, importe o framework necessário usando `import Foundation`.
2. Em seguida, defina a URL da página da web que deseja baixar, por exemplo: `let urlString = "https://www.example.com"`.
3. Converta a URL para um objeto do tipo `URL` usando `let url = URL(string: urlString)`.
4. Agora você precisa criar um objeto de sessão usando `let session = URLSession(configuration: .default)`.
5. Em seguida, crie uma tarefa de download usando `let task = session.dataTask(with: url, completionHandler: { (data, response, error) in ... })`.
6. No encerramento da tarefa, você pode acessar os dados da página baixada, a resposta do servidor e possíveis erros.
7. Por fim, você pode converter os dados em uma string usando `let htmlString = String(data: data, encoding: .utf8)`.
8. Agora você pode usar a string para exibir o conteúdo da página em seu aplicativo.

Veja abaixo um exemplo de código completo:

```Swift
import Foundation

let urlString = "https://www.example.com"
let url = URL(string: urlString)
let session = URLSession(configuration: .default)
let task = session.dataTask(with: url, completionHandler: { (data, response, error) in
    if let data = data, let htmlString = String(data: data, encoding: .utf8) {
        // use the html string to display the webpage in your app
    }
})
task.resume()
```

## Profundidade

Ao baixar uma página da web, é importante considerar algumas coisas. Primeiro, certifique-se de estar utilizando uma URL segura (HTTPS) para garantir a segurança dos dados baixados. Além disso, você pode personalizar sua tarefa de download, adicionando cabeçalhos ou definindo limites de tempo.

Você também pode utilizar o framework `WebKit` para exibir a página da web diretamente em uma WebView em seu aplicativo.

## Veja também

- [Swift Documentation](https://developer.apple.com/documentation/swift)
- [Foundation framework reference](https://developer.apple.com/documentation/foundation)
- [URLSession class reference](https://developer.apple.com/documentation/foundation/urlsession)
- [NSURLSession class reference](https://developer.apple.com/documentation/foundation/nsurlsession)
- [WebKit framework reference](https://developer.apple.com/documentation/webkit)