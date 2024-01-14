---
title:                "Swift: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

Convertendo uma Visão Geral sobre Saída de Debug em Swift

## Por que?

Se você é um desenvolvedor Swift experiente ou está começando a aprender a linguagem, provavelmente já ouviu falar sobre a importância de imprimir saída de debug em seu código. Mas por que isso é necessário?

Imprimir saída de debug é uma técnica essencial para entender como seu código está sendo executado. Isso permite que você veja os valores das variáveis ​​e os resultados das operações em tempo real, facilitando a detecção de erros e a solução de problemas em seu código. Além disso, a saída de debug pode ser útil para entender o fluxo de execução do seu código, especialmente em projetos complexos.

## Como Fazer?

Em Swift, a impressão de saída de debug é feita com o uso da função `print()`, que aceita um ou mais argumentos e os exibe no console de depuração. Aqui está um exemplo simples:

```Swift
var nome = "João"
var idade = 30

print(nome, idade)

Output: João 30
```

Note que os valores das variáveis ​​são impressos no console na mesma ordem em que foram passados ​​para a função `print()`.

Você também pode utilizar a interpolação de strings para imprimir o valor de uma variável dentro de uma string. Por exemplo:

```Swift
var nome = "Maria"
var idade = 25

print("Olá, meu nome é \(nome) e eu tenho \(idade) anos.")

Output: Olá, meu nome é Maria e eu tenho 25 anos.
```

Além disso, a função `print()` também aceita argumentos opcionais, como `separator` e `terminator`, que podem ser úteis para formatar a saída de debug. Por exemplo:

```Swift
var nome = "Pedro"
var idade = 35

print(nome, idade, separator: " - ", terminator: "!")
// O separador será aplicado entre os argumentos e o terminador será exibido no final da saída.

Output: Pedro - 35!
```

## Mergulho Profundo

Agora que você sabe como imprimir saída de debug em Swift, vale a pena lembrar que essa técnica pode ser aplicada não apenas em variáveis, mas também em expressões e resultados de chamadas de função. Além disso, você pode utilizar a função `dump()` para imprimir informações detalhadas sobre um objeto específico.

Também é importante lembrar que a saída de debug é útil durante o processo de desenvolvimento, mas não deve ser deixada em seu código final. Certifique-se de removê-la antes de fazer o build de sua aplicação.

## Veja Também

Para mais informações sobre saída de debug em Swift, confira os links abaixo:

[Documentação oficial do Swift: Depuração e Saída de Debug](https://docs.swift.org/swift-book/LanguageGuide/Debugging.html)

[Vídeo Tutorial: Depuração de Código Swift](https://www.youtube.com/watch?v=5Lbi-mqqcd8)

[Tutorial: Depuração de Erros em Swift com Xcode](https://agostini.tech/2018/11/12/depuracao-erros-swift-xcode/)