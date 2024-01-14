---
title:                "Swift: Imprimindo saída de depuração"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

A programação pode ser uma tarefa desafiadora e, às vezes, pode ser difícil identificar onde um erro está ocorrendo. É aí que o recurso de impressão de saída de depuração se torna extremamente útil. Com ele, podemos visualizar informações importantes durante a execução do código, facilitando a identificação de possíveis problemas.

## Como Fazer

Para imprimir saída de depuração em Swift, usamos a função `print()`. Essa função pode receber como parâmetro diferentes tipos de dados, como strings, números, booleanos e até mesmo objetos. Veja um exemplo abaixo:

```Swift
let nome = "João"
let idade = 28
let altura = 1.75
print("O nome do usuário é \(nome), sua idade é \(idade) e sua altura é \(altura)m.")
```

A saída dessa função seria: `O nome do usuário é João, sua idade é 28 e sua altura é 1.75m.`

Além disso, também podemos utilizar a função `debugPrint()` para imprimir informações extras, como a representação literal de objetos. Por exemplo:

```Swift
class Carro {
   var marca: String
   var modelo: String
   init(marca: String, modelo: String) {
       self.marca = marca
       self.modelo = modelo
   }
}

let carro1 = Carro(marca: "Fiat", modelo: "Uno")
debugPrint(carro1)
```

A saída dessa função seria: `Carro(marca: "Fiat", modelo: "Uno")`, mostrando a representação literal do objeto `carro1`.

## Mais Profundo

Além das funções `print()` e `debugPrint()`, também podemos utilizar a função `dump()` para imprimir a quantidade exata de informações de um objeto. Essa função é útil quando precisamos analisar um objeto mais complexo e obter uma visão detalhada de seus valores e propriedades. Por exemplo:

```Swift
struct Pessoa {
   var nome: String
   var idade: Int
   var endereço: Endereço
}
struct Endereço {
   var rua: String
   var número: Int
   var cidade: String
}

let pessoa1 = Pessoa(nome: "Maria", idade: 35, endereço: Endereço(rua: "Rua A", número: 123, cidade: "São Paulo"))
dump(pessoa1)
```

A saída dessa função seria:

```
▿ Pessoa
   - nome: "Maria"
   - idade: 35
   ▿ endereço: Endereço
      - rua: "Rua A"
      - número: 123
      - cidade: "São Paulo"
```

Essa função nos dá uma visão mais profunda dos valores e propriedades do objeto `pessoa1`, facilitando a análise de informações complexas.

## Veja Também

- [Documentação oficial do `print()` em Swift](https://developer.apple.com/documentation/swift/string/3127549-print)
- [Documentação oficial do `dump()` em Swift](https://developer.apple.com/documentation/swift/swift_standard_library/debugging_and_logging/dump)