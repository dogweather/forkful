---
title:                "Escrevendo para o erro padrão."
html_title:           "Swift: Escrevendo para o erro padrão."
simple_title:         "Escrevendo para o erro padrão."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever para o erro padrão é uma técnica usada por programadores para relatar erros ou mensagens importantes durante a execução de um programa. Isso permite que o programador tenha mais controle sobre quais informações são exibidas para o usuário. 

## Como fazer:

Para escrever para o erro padrão em Swift, basta usar o comando ```Swift print() ``` e especificar ```Swift stderr ``` como o parâmetro para o fluxo de saída. Por exemplo: 

```Swift
print("Erro:", to: &stderr)
```

Isso irá imprimir a mensagem "Erro" no console de erro. Você também pode passar variáveis como argumento para exibir informações específicas:

```Swift
let idade = 18
print("Você tem \(idade) anos.", to: &stderr)
```

Isso irá imprimir "Você tem 18 anos." no console de erro. 

## Profundidade:

Escrever para o erro padrão é uma prática comum em programação, comumente usada em linguagens como Swift, C++ e Python. Foi introduzida como uma alternativa à exibição de mensagens no console principal, permitindo que o programador organize melhor as informações para o usuário. Além disso, escrever para o erro padrão também pode ser útil ao escrever scripts de linha de comando ou aplicativos que precisam ter um controle mais detalhado sobre as mensagens exibidas. 

## Veja também:

- [Documentação oficial do Swift](https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html)
- [Artigo sobre escrita para o erro padrão em Python](https://realpython.com/python-logging/)