---
title:                "Trabalhando com CSV"
date:                  2024-01-19
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com CSV (Comma-Separated Values) significa manipular dados em formato de texto com valores separados por vírgula. Programadores fazem isso por ser uma maneira simples e amplamente suportada para importar e exportar tabular dados para bancos de dados, planilhas e outras ferramentas.

## Como Fazer:
```Swift
import Foundation

// Simulando um CSV
let rawCSV = """
Nome,Idade,Cidade
João,28,Rio de Janeiro
Clara,34,São Paulo
Tiago,23,Curitiba
"""

// Dividindo o CSV em linhas
let rows = rawCSV.components(separatedBy: "\n")

// Iterator para pular o cabeçalho
var firstLine = true

for row in rows {
    guard !firstLine else {
        firstLine = false
        continue
    }

    let columns = row.components(separatedBy: ",")
    print("Nome: \(columns[0]), Idade: \(columns[1]), Cidade: \(columns[2])")
}
```

Saída de exemplo:
```
Nome: João, Idade: 28, Cidade: Rio de Janeiro
Nome: Clara, Idade: 34, Cidade: São Paulo
Nome: Tiago, Idade: 23, Cidade: Curitiba
```

## Aprofundando:
O CSV é um formato antigo, originado na década de 1970 e permanece popular devido à sua simplicidade. Alternativas como XML e JSON oferecem mais recursos e estrutura, mas às vezes são mais complexos do que o necessário. Ao trabalhar com CSV em Swift, você pode utilizar o `String` e `Array` APIs para analisar e manipular os dados, ou recorrer a frameworks específicos para funcionalidades avançadas e melhor gestão de erros.

## Veja Também:
- [Documentação da Apple sobre Foundation](https://developer.apple.com/documentation/foundation)
- [CSV.swift](https://github.com/yaslab/CSV.swift): Uma biblioteca Swift para leitura e escrita de arquivos CSV.
- [Tutorial sobre Parsing de JSON em Swift](https://developer.apple.com/swift/blog/?id=37)
