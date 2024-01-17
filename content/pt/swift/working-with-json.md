---
title:                "Trabalhando com json"
html_title:           "Swift: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-json.md"
---

{{< edit_this_page >}}

## O que é e por que se trabalha com JSON

JSON (JavaScript Object Notation) é um formato de dados comumente usado para transmitir e armazenar informações na programação. Os programadores trabalham com JSON porque ele é simples, leve e amplamente suportado em diferentes plataformas e linguagens de programação.

## Como fazer:

1. Importe o módulo JSON no seu projeto em Swift:
```Swift
import Foundation
```

2. Converta um objeto Swift para JSON:
```Swift
// Criando um objeto Swift
let pessoa = [
    "nome": "João",
    "idade": 28,
    "profissao": "Desenvolvedor"
]

// Convertendo o objeto para JSON
let json = try? JSONSerialization.data(withJSONObject: pessoa, options: [])

// Imprimindo o resultado
print(String(data: json, encoding: .utf8)!) // {"nome":"João","idade":28,"profissao":"Desenvolvedor"}
```

3. Converta um JSON para objeto Swift:
```Swift
// Definindo uma string com um JSON
let jsonStr = """
{
    "nome": "Maria",
    "idade": 30,
    "profissao": "Designer"
}
"""

// Convertendo o JSON para objeto Swift
if let data = jsonStr.data(using: .utf8), let pessoa = try? JSONSerialization.jsonObject(with: data, options: []) as? [String:Any] {
    print(pessoa) // ["nome": "Maria", "idade": 30, "profissao": "Designer"]
}
```

## Mergulho profundo:

1. Contexto histórico: JSON foi criado por Douglas Crockford em 2001 e se tornou popular devido a sua simplicidade e eficiência em comparação com outros formatos de dados.

2. Alternativas: existem outros formatos de dados que podem ser usados em vez de JSON, como XML, YAML e CSV. Cada um tem suas próprias características e é usado para diferentes propósitos.

3. Detalhes de implementação: em Swift, é possível trabalhar com JSON a partir do módulo Foundation ou usando bibliotecas externas como SwiftyJSON e Codable.


## Veja também:

- [Documentação oficial do JSON em Swift](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Tutorial em vídeo sobre como trabalhar com JSON em Swift](https://www.youtube.com/watch?v=rMgFoakcBr0)
- [Biblioteca SwiftyJSON para facilitar o trabalho com JSON em Swift](https://github.com/SwiftyJSON/SwiftyJSON)