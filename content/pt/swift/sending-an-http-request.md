---
title:                "Swift: Enviando uma Solicitação http"
simple_title:         "Enviando uma Solicitação http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma requisição HTTP?

A utilização de requisições HTTP é fundamental em desenvolvimento de aplicativos, permitindo a comunicação com servidores para obter, enviar ou alterar dados. Isso é essencial para que seus aplicativos sejam capazes de acessar informações externas e fornecer uma melhor experiência para o usuário.

## Como fazer:

Enviar uma requisição HTTP em Swift é bastante simples. Veja um exemplo abaixo:

```Swift
let url = URL(string: "https://api.example.com/users")
let session = URLSession.shared
let task = session.dataTask(with: url!) { (data, response, error) in
    guard let data = data else { return }
    do {
        let response = try JSONDecoder().decode(User.self, from: data)
        // Use os dados recebidos aqui
    } catch let error {
        print(error)
    }
}
task.resume()
```

No exemplo acima, é criada uma NSURLSession com o URL fornecido. Em seguida, é realizado uma tarefa de dados (dataTask) com o URL, e uma cláusula de fechamento (closure) para processar os dados recebidos. O retorno de dados é convertido para o tipo de objeto especificado (User, neste caso) usando JSONDecoder e está pronto para ser usado.

## Mergulho profundo:

Ao enviar uma requisição HTTP, é importante entender os diferentes tipos de métodos (GET, POST, PUT, DELETE) e quais dados são esperados como resposta. Além disso, é importante estar ciente dos diferentes formatos de dados, como JSON, XML ou YAML, e escolher o mais adequado para o seu aplicativo.

Além disso, é importante considerar a segurança ao enviar uma requisição HTTP. Certifique-se de usar uma conexão segura (HTTPS) e adicionar autenticação para proteger os dados sensíveis.

## Veja também:

- [Documentação oficial da Foundation para URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Como fazer uma requisição HTTP em Swift](https://www.hackingwithswift.com/read/32/2/making-http-requests-with-swift-3-urlsession-cookies-and-authentication)

---

Obrigado por ler! Esperamos que este artigo tenha sido útil e que você agora se sinta confiante em enviar requisições HTTP em seus aplicativos Swift. Se tiver alguma dúvida ou sugestão, sinta-se à vontade para deixar um comentário abaixo.