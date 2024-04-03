---
date: 2024-01-19
description: "Trabalhar com JSON (JavaScript Object Notation) significa manipular\
  \ dados no formato leve de interc\xE2mbio que \xE9 f\xE1cil de ler para humanos\
  \ e simples para\u2026"
lastmod: '2024-03-13T22:44:46.942260-06:00'
model: unknown
summary: "Trabalhar com JSON (JavaScript Object Notation) significa manipular dados\
  \ no formato leve de interc\xE2mbio que \xE9 f\xE1cil de ler para humanos e simples\
  \ para m\xE1quinas processarem."
title: Trabalhando com JSON
weight: 38
---

## O Que é & Porquê?
Trabalhar com JSON (JavaScript Object Notation) significa manipular dados no formato leve de intercâmbio que é fácil de ler para humanos e simples para máquinas processarem. Programadores usam JSON para transmitir dados entre um servidor e um cliente na web, além de salvar configurações e preferências dentro das aplicações.

## Como Fazer:
Em Swift, você usa `JSONDecoder` para converter JSON em modelos Swift e `JSONEncoder` para fazer o contrário. Suponhamos que você tem um JSON representando um usuário:

```Swift
let jsonString = """
{
    "nome": "João",
    "idade": 28,
    "email": "joao@example.com"
}
"""

struct Usuario: Codable {
    var nome: String
    var idade: Int
    var email: String
}

let jsonData = jsonString.data(using: .utf8)!
let decoder = JSONDecoder()

do {
    let usuario = try decoder.decode(Usuario.self, from: jsonData)
    print(usuario)
} catch {
    print(error.localizedDescription)
}
```

Se tudo der certo, verás algo assim no console:

```Swift
Usuario(nome: "João", idade: 28, email: "joao@example.com")
```

Para converter um modelo Swift em JSON, use `JSONEncoder`:

```Swift
let encoder = JSONEncoder()
if let data = try? encoder.encode(usuario), 
   let jsonString = String(data: data, encoding: .utf8) {
    print(jsonString)
}
```

E no console:

```Swift
{"nome":"João","idade":28,"email":"joao@example.com"}
```

## Mergulho Profundo:
JSON é baseado em JavaScript, mas é independente de linguagem, usável em muitas plataformas. Surgiu na década de 2000 como uma alternativa ao XML, sendo mais enxuto e fácil de utilizar. Alternativas modernas ao JSON incluem BSON e Protocol Buffers, cada um com suas vantagens em performance e recursos. Ao trabalhar com Swift, o protocolo `Codable` facilita a serialização de modelos, mas não esqueça de lidar com os erros que podem surgir ao decodificar JSON que não corresponde aos modelos Swift esperados.

## Veja Também:
- Documentação oficial do Swift sobre `Codable`: https://developer.apple.com/documentation/swift/codable
- JSON.org, com a especificação e exemplos de JSON: https://www.json.org/json-pt.html
- Um artigo sobre os benefícios e limitações do JSON em comparação com XML: https://www.smashingmagazine.com/2020/11/advantages-json-xml/
