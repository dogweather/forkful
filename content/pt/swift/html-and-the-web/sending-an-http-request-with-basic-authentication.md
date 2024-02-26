---
date: 2024-01-20 18:02:45.784615-07:00
description: "Em Swift, enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1\
  sica significa passar um usu\xE1rio e senha para acessar um recurso protegido na\
  \ web. Programadores\u2026"
lastmod: '2024-02-25T18:49:44.537826-07:00'
model: gpt-4-1106-preview
summary: "Em Swift, enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1\
  sica significa passar um usu\xE1rio e senha para acessar um recurso protegido na\
  \ web. Programadores\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Em Swift, enviar uma requisição HTTP com autenticação básica significa passar um usuário e senha para acessar um recurso protegido na web. Programadores fazem isso para interagir com APIs que requerem identificação segura.

## Como Fazer:
```Swift
import Foundation

let username = "usuario"
let password = "senha"
let loginString = "\(username):\(password)"
guard let loginData = loginString.data(using: .utf8) else { return }
let base64LoginString = loginData.base64EncodedString()

var request = URLRequest(url: URL(string: "https://exemplo.com/api/dados")!)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "Resposta desconhecida")
        return
    }

    if let httpStatus = response as? HTTPURLResponse, httpStatus.statusCode != 200 {
        print("Status HTTP: \(httpStatus.statusCode)")
        return
    }

    // Trabalhando com os dados recebidos
    if let resultado = String(data: data, encoding: .utf8) {
        print(resultado)
    }
}.resume()
```
Saída de exemplo:
```
{
  "dados": "Informações protegidas que você requisitou."
}
```

## Aprofundamento
Autenticação básica HTTP é um método antigo para enviar credenciais. Ela embala o usuário e senha em Base64, mas não é cifrada, então é vulnerável se não usada com HTTPS. Alternativas seguras incluem tokens de acesso e OAuth. Na implementação, garanta que a requisição tenha o cabeçalho correto e use HTTPS para evitar exposição das credenciais. Em Swift, `URLRequest` lida com o cabeçalho enquanto `URLSession` administra o envio.

## Veja Também
- [Documentação da Apple sobre URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
