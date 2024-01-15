---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Swift: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que
Você provavelmente está se perguntando por que alguém se importaria em enviar uma solicitação HTTP com autenticação básica. Bem, este é um método comum e seguro para se autenticar em um servidor e acessar informações protegidas por um nome de usuário e senha.

## Como
Para enviar uma solicitação HTTP com autenticação básica em Swift, você precisará usar o `URLSession` e o `URLRequest`, além de adicionar as credenciais de autenticação no cabeçalho da solicitação. Confira o exemplo abaixo:

``` Swift
if let url = URL(string: "exemplo.com") {
    var request = URLRequest(url: url)
    let username = "seunome"
    let password = "suasenha"
    let loginString = "\(username):\(password)"
    let loginData = loginString.data(using: .utf8)
    guard let base64LoginString = loginData?.base64EncodedString() else { return }

    request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        guard let data = data,
            let response = response as? HTTPURLResponse,
            error == nil else { return }
        
        if response.statusCode == 200 {
            // Sucesso! Você está autenticado e pode acessar as informações protegidas.
            print(data)
        } else {
            // Houve algum erro na autenticação. Verifique suas credenciais ou tente novamente mais tarde.
        }
    }

    task.resume()
}
```

## Deep Dive
A autenticação básica é um método de autenticação que envolve o envio de um nome de usuário e senha no cabeçalho da solicitação HTTP. Essas credenciais são codificadas em base64 antes de serem enviadas, o que as torna mais seguras do que simplesmente passar o nome de usuário e senha em texto simples.

## Veja também
- [Documentação oficial da Apple sobre `URLSession`](https://developer.apple.com/documentation/foundation/urlsession)
- [Explicação detalhada da autenticação básica](https://pt.wikipedia.org/wiki/Basic_access_authentication)