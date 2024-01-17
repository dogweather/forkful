---
title:                "Imprimindo saída de depuração"
html_title:           "Swift: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e Porque? 

 Imprimir a saída de debug é uma técnica comumente usada por programadores para imprimir informações durante o desenvolvimento de um aplicativo. Isso pode ser útil para verificar o valor de uma variável ou entender o fluxo de execução do código.

## Como Fazer: 

 Para imprimir mensagens de debug em Swift, use a função print() com o valor ou a variável que você deseja imprimir entre parênteses. Por exemplo:

 Swift
```
print("Olá mundo!")
```

A saída será "Olá mundo!" na console do Xcode. Também é possível imprimir múltiplos valores separando-os por vírgula, como mostrado abaixo:

 Swift
```
let nome = "Pedro"
let idade = 25
print("Meu nome é", nome, "e tenho", idade, "anos.")
```

Isso imprimirá "Meu nome é Pedro e tenho 25 anos." na console.

## Mergulho Profundo: 

 Imprimir mensagens de debug não é uma técnica nova, mas tem sido um recurso importante para programadores desde o início da linguagem de programação. Existem também métodos mais avançados de debug, como o uso de breakpoints e ferramentas de depuração, mas imprimir saída de debug ainda é uma maneira simples e eficaz de entender o comportamento de um programa.

## Veja Também: 

 Para saber mais sobre como imprimir saída de debug em Swift, confira a documentação oficial da Apple sobre a função print() (https://developer.apple.com/documentation/swift/1540833-print). Você também pode encontrar informações e dicas úteis em fóruns e comunidades de programação online. Experimente usar a função print() em seu próximo projeto para facilitar o processo de debug!