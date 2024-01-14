---
title:    "Swift: Lendo argumentos da linha de comando"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade muito importante para programadores Swift. Com isso, é possível criar programas interativos e dinâmicos, permitindo que o usuário forneça informações para personalizar a execução do programa. Além disso, a leitura de argumentos da linha de comando é extremamente útil em projetos que fazem uso de bibliotecas ou frameworks externos, onde pode ser necessário passar informações específicas durante a execução do programa.

## Como ler argumentos da linha de comando?

Para ler argumentos da linha de comando em Swift, podemos usar a classe `CommandLine`, que fornece métodos para acessar e manipular os argumentos passados na execução do programa. Primeiro, devemos importar o módulo `Foundation` para ter acesso à classe `CommandLine`:

```
import Foundation
```

Em seguida, podemos acessar os argumentos usando a propriedade `arguments` da classe `CommandLine`. Por exemplo, para imprimir todos os argumentos passados ao programa, podemos utilizar o seguinte código:

```
for argument in CommandLine.arguments {
    print(argument)
}
```

Ao executar o programa com os argumentos `hello` e `world`, o resultado será:

```
hello
world
```

## Aprofundando-se na leitura de argumentos da linha de comando

Além da propriedade `arguments`, a classe `CommandLine` também possui outros métodos úteis para acessar e manipular os argumentos passados ao programa. Alguns exemplos incluem `argumentCount`, que retorna o número total de argumentos, e `firstArgumentIgnoringFlags`, que retorna o primeiro argumento ignorando possíveis opções passadas com o prefixo `-`.

Outra funcionalidade importante é a possibilidade de definir opções e parâmetros esperados pelo programa, bem como suas descrições, através do método `defineOption`. Isso pode ser especialmente útil em programas mais complexos que exigem a leitura de vários argumentos com diferentes formatos e propósitos.

Para saber mais sobre como usar a classe `CommandLine`, consulte a documentação oficial da Apple [aqui](https://developer.apple.com/documentation/foundation/commandline).

## Veja também

- [Documentação oficial da Apple - CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Swiftpack - Como ler argumentos da linha de comando em Swift](https://swiftpack.co/package/JohnSundell/CommandLine)