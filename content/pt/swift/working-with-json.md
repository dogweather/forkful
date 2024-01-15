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

## Por que

Se você está trabalhando com dados em um aplicativo iOS, é provável que em algum momento você precise lidar com JSON. JSON (JavaScript Object Notation) é um formato de armazenamento e troca de dados simples, leve e amplamente utilizado, o que o torna uma opção popular para comunicação entre aplicativos e servidores.

## Como fazer

Para trabalhar com JSON em um aplicativo iOS usando Swift, existem algumas etapas que você precisará seguir.

### 1. Importar o framework `Foundation`

Para começar, você precisará importar o framework `Foundation` no seu arquivo de código. Isso permitirá que você use as classes e métodos fornecidos pela Apple para manipulação de dados.

```Swift
import Foundation
```

### 2. Obter dados JSON de uma URL

Em seguida, você precisará obter os dados JSON de uma URL. Você pode fazer isso usando a função `data(contentsOf:)`, que permite que você faça uma solicitação simples e obtenha os dados diretamente.

```Swift
if let url = URL(string: "https://sample-url.com/data.json") {
    do {
        let data = try Data(contentsOf: url)
        // aqui você pode manipular os dados obtidos
    } catch {
        // lidar com erros de solicitação ou parse dos dados
    }
}
```

### 3. Decodificar os dados JSON

Depois de obter os dados, você precisará decodificá-los usando o `JSONDecoder` e mapeá-los para uma estrutura de dados adequada. Por exemplo, se seus dados JSON forem um array de objetos, você pode criar uma estrutura que represente esses objetos e usar o `JSONDecoder` para decodificá-los automaticamente.

```Swift
// exemplo de uma estrutura de dados para mapeamento de objetos JSON
struct Person: Codable {
    let name: String
    let age: Int
}

// decodificando os dados e armazenando-os em uma variável
let decoder = JSONDecoder()
do {
    let person = try decoder.decode([Person].self, from: data)
    // aqui você pode usar os dados decodificados como desejar
} catch {
    // lidar com erros de decodificação
}
```

### 4. Usar os dados decodificados

Agora que você tem os dados decodificados, pode usá-los em seu aplicativo como desejar. Você pode exibi-los em uma tabela, salvar em um banco de dados local ou usar para fazer solicitações adicionais.

## Mergulho profundo

Além das etapas básicas acima, há muito mais que você pode fazer com JSON em um aplicativo iOS. Alguns tópicos adicionais incluem a manipulação de formatos de data, a autenticação com servidores usando JSON e a criação de APIs RESTful para o seu aplicativo.

Veja também:

- [Documentação oficial sobre JSON em Swift](https://developer.apple.com/documentation/foundation/json)
- [Tutorial de introdução ao JSON em Swift da Ray Wenderlich](https://www.raywenderlich.com/6585047-ios-json-tutorial-getting-started)