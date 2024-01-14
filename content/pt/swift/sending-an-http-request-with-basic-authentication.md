---
title:                "Swift: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##Por que

Enviar uma solicitação HTTP com autenticação básica é uma maneira essencial de se comunicar com servidores e consumir dados através da internet. É uma forma segura e eficiente de garantir que o usuário tenha acesso apenas às informações necessárias.

##Como Fazer

Para enviar uma solicitação HTTP com autenticação básica em Swift, siga os seguintes passos:

1. Importe o framework "Foundation" no seu projeto.

2. Crie uma instância da classe URLRequest para representar sua solicitação. Você pode fornecer a URL desejada e escolher o método HTTP a ser usado (GET, POST, PUT, etc.).

```Swift
let request = URLRequest(url: URL(string: "https://www.exemplo.com/api/dados")!)
```

3. Crie uma instância da classe URLSessionConfiguration para configurar sua sessão de rede. Defina o tipo de autenticação desejado como `httpBasic`.

```Swift
let config = URLSessionConfiguration.default
config.httpAdditionalHeaders = ["Authorization": "Basic usuário:senha"]
```

4. Crie uma instância da classe URLSession e passe a configuração que acabou de criar como parâmetro.

```Swift
let session = URLSession(configuration: config)
```

5. Chame o método `dataTask` da sua instância de `URLSession`, passando a solicitação que criou e um closure que será executado depois que a solicitação for concluída.

```Swift
let task = session.dataTask(with: request) { (data, response, error) in
    if let erro = erro {
        print("Erro ao enviar solicitação: \(erro)")
    } else {
        // Faça o que precisar com os dados e a resposta recebidos
    }
}
```

6. Inicie a tarefa, chamando o método `resume()`.

```Swift
task.resume()
```

##Deep Dive

No passo 3, usamos a autenticação básica do HTTP, que envolve o envio de um nome de usuário e senha em formato base64 codificado no header `Authorization` da solicitação. Esta forma de autenticação é simples e amplamente suportada pelos servidores.

Porém, é importante ressaltar que essa autenticação só deve ser usada em conexões HTTPS, já que o username e senha são enviados em formato texto legível.

##Veja Também

- [Documentação da Apple sobre URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Tutorial sobre URLRequests e URLSession em Swift](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)