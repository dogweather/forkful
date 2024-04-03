---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:29.637902-07:00
description: "Como fazer: Em Swift, n\xE3o h\xE1 suporte nativo para a an\xE1lise\
  \ de arquivos CSV diretamente, mas voc\xEA pode manipular dados CSV usando os m\xE9\
  todos de `String`\u2026"
lastmod: '2024-03-13T22:44:46.943231-06:00'
model: gpt-4-0125-preview
summary: "Em Swift, n\xE3o h\xE1 suporte nativo para a an\xE1lise de arquivos CSV\
  \ diretamente, mas voc\xEA pode manipular dados CSV usando os m\xE9todos de `String`\
  \ para dividir o conte\xFAdo, ou por meio de bibliotecas de terceiros como SwiftCSV\
  \ para uma abordagem mais simplificada."
title: Trabalhando com CSV
weight: 37
---

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
