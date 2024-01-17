---
title:                "Criando um arquivo temporário"
html_title:           "Swift: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e por que?
Criar um arquivo temporário, como o nome sugere, é criar um arquivo que só é necessário por um curto período de tempo e será deletado assim que a tarefa for concluída. Programadores geralmente criam arquivos temporários para armazenar dados temporários que serão usados ​​em seu código. Isso ajuda a manter o código organizado e evita a sobrecarga de dados desnecessários.

## Como fazer:
````Swift 
let temporaryURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("meuArquivoTemporario.txt")

do {
  try "Hello World".write(to: temporaryURL, atomically: true, encoding: .utf8)
  print("Arquivo temporário criado com sucesso!")
} catch {
  print("Erro ao criar arquivo temporário: \(error.localizedDescription)")
}
````
 Output: Arquivo temporário criado com sucesso!

## Deep Dive:
Criar arquivos temporários é comum na programação desde os primeiros dias da computação. Antes da existência de sistemas de arquivos de alto nível, programadores criavam arquivos temporários diretamente em memória. Hoje em dia, existem alternativas para a criação de arquivos temporários, como o uso de caches e bancos de dados. Em Swift, também é possível criar diretamente um arquivo temporário sem a necessidade de especificar seu caminho completo.

## See Also:
Para mais informações sobre a criação de arquivos temporários em Swift, consulte a documentação oficial em: https://developer.apple.com/documentation/foundation/nsfilemanager/1407737-
url_forcreatesubdirectory