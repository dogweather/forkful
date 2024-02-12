---
title:                "Trabalhando com CSV"
aliases:
- /pt/swift/working-with-csv/
date:                  2024-02-03T19:21:29.637902-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Trabalhar com arquivos CSV (Valores Separados por Vírgula) envolve analisar e gerar dados estruturados a partir de arquivos de texto onde cada linha representa um registro e cada registro consiste em campos separados por vírgulas. Programadores frequentemente se engajam nessa atividade para facilmente importar, exportar e manipular dados tabulares usando um formato que é amplamente suportado através de diferentes plataformas e linguagens de programação, devido à sua simplicidade e formato legível por humanos.

## Como fazer:

Em Swift, não há suporte nativo para a análise de arquivos CSV diretamente, mas você pode manipular dados CSV usando os métodos de `String` para dividir o conteúdo, ou por meio de bibliotecas de terceiros como SwiftCSV para uma abordagem mais simplificada. Aqui estão ambos os métodos:

### Análise Manual sem Bibliotecas Externas
```swift
// Considere uma simples string CSV
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Divida a string CSV em linhas
let rows = csvString.components(separatedBy: "\n")

// Extraia as chaves da primeira linha
let keys = rows.first?.components(separatedBy: ",")

// Itere sobre as linhas começando da segunda
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// Saída de Exemplo
print(result)
// Saída: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
Essa abordagem é direta, mas carece de robustez, especialmente com arquivos CSV contendo casos especiais como vírgulas nos valores, quebras de linha dentro dos campos, etc.

### Usando a Biblioteca SwiftCSV
Primeiro, adicione SwiftCSV ao seu projeto incluindo-o em suas dependências do `Package.swift`:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Então, importe e use da seguinte forma:
```swift
import SwiftCSV

// Assuma que `csvString` esteja definido como acima

// Crie um objeto CSV
if let csv = try? CSV(string: csvString) {
    // Acesse as linhas como dicionários
    let rows = csv.namedRows
    
    // Saída de Exemplo
    print(rows)
    // Saída: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV simplifica a análise ao lidar automaticamente com nuances como vírgulas encapsuladas, quebras de linha em campos e codificação de caracteres. No entanto, lembre-se de tratar possíveis erros em aplicações do mundo real, especialmente ao lidar com fontes de dados externas.
