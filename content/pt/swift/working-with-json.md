---
title:                "Swift: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

Se você já trabalha com programação em Swift ou está pensando em aprender, provavelmente já ouviu falar sobre JSON (JavaScript Object Notation). JSON é um formato de dados amplamente utilizado no desenvolvimento de aplicativos para troca de informações entre diferentes plataformas e linguagens de programação.

Por que é importante saber como trabalhar com JSON? Porque ele é um formato simples, leve e eficiente para transferência de dados e é amplamente suportado por diversas tecnologias e plataformas. Além disso, muitas APIs de serviços populares, como Google, Facebook e Twitter, utilizam JSON para enviar e receber dados.

## Como trabalhar com JSON em Swift

Para trabalhar com JSON em Swift, primeiro é necessário importar a biblioteca `Foundation` no início do código. Em seguida, é preciso utilizar o método `JSONSerialization` para converter os dados JSON em estruturas nativas do Swift.

Um exemplo seria o seguinte código, que lê um arquivo JSON local e imprime o conteúdo na tela:

```Swift
// Importando a biblioteca Foundation
import Foundation

// Caminho do arquivo JSON local
let fileURL = Bundle.main.url(forResource: "exemplo", withExtension: "json")

// Lendo o conteúdo do arquivo
do {
    let data = try Data(contentsOf: fileURL!)

    // Convertendo os dados em estruturas nativas do Swift
    if let json = try JSONSerialization.jsonObject(with: data, options: .allowFragments) as? [String: Any] {
        // Imprimindo o conteúdo do arquivo JSON
        print(json)
    }
} catch {
    print("Erro ao ler o arquivo JSON!")
}
```

O resultado dessa execução seria a impressão do conteúdo do arquivo JSON, que pode ser acessado utilizando o nome das chaves e a sintaxe de dicionário do Swift.

## Aprofundando-se em JSON com Swift

Ao trabalhar com JSON em Swift, é importante entender a estrutura desse formato de dados. Ele é composto por pares de chave-valor, onde a chave é sempre uma string e pode representar um nome ou uma propriedade, e o valor pode ser uma string, número, booleano, array ou outro objeto JSON.

Além disso, é possível utilizar diferentes métodos para transformar os dados JSON em estruturas nativas do Swift, como `JSONSerialization` e `Codable`. O `Codable` é uma opção mais recente e eficiente, pois permite mapear os dados diretamente para estruturas de dados criadas pelo desenvolvedor.

Outra parte importante do trabalho com JSON em Swift é o tratamento de erros. É sempre importante garantir que o arquivo JSON esteja no formato correto e manipular possíveis erros, como arquivo ausente ou dados incompatíveis.

## Veja também

Aqui estão alguns links úteis para saber mais sobre trabalhar com JSON em Swift:

- [Documentação oficial do Swift sobre JSON](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Trabalhando com JSON em aplicativos iOS](https://www.appcoda.com.br/aprenda-como-trabalhar-com-json-em-swift/)
- [Convertendo estruturas do Swift em JSON com Codable](https://medium.com/@svnt/convertendo-estruturas-do-swift-em-json-com-codable-848565f0ef2c)