---
title:                "Swift: Unindo strings"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Se você está aprendendo a programar em Swift, é importante entender como concatenar strings para criar mensagens personalizadas para o seu aplicativo. Concatenar strings é uma maneira útil de combinar várias informações em uma única string para exibir aos usuários ou usar em cálculos.

## Como fazer

Existem algumas maneiras de concatenar strings em Swift, dependendo de suas necessidades. Vamos dar uma olhada em alguns exemplos de código usando o operador `+`:

```Swift
var mensagem = "Bem-vindo "
var nome = "Fernanda"
var sobrenome = "Silva"

mensagem = mensagem + nome + " " + sobrenome
print(mensagem)
```

A saída será:

```
Bem-vindo Fernanda Silva
```

Você também pode usar o método `append()` para adicionar strings ao final de uma string existente. Por exemplo:

```Swift
let texto = "Eu adoro "
let linguagem = "programação"
var frase = texto.append(linguagem)

print(frase)
```

A saída será:

```
Eu adoro programação
```

Se você precisar de mais controle sobre como as strings estão sendo concatenadas, pode usar o método `join()` com um array de strings. Por exemplo:

```Swift
let nomes = ["Maria", "João", "Ana", "Pedro"]
let saudacao = "Olá, meu nome é"
var mensagem = saudacao.join(nomes)

print(mensagem)
```

A saída será:

```
Olá, meu nome é Maria, João, Ana, Pedro
```

## Aprofundando

Concatenar strings pode parecer simples, mas existem algumas coisas que você deve estar ciente para evitar problemas. Por exemplo, quando você está concatenando várias strings, é importante ter cuidado com a ordem em que as strings são adicionadas. Como vimos no primeiro exemplo acima, a primeira string que adicionamos foi "Bem-vindo", mas se tivéssemos adicionado o nome antes, a saída seria "Fernanda Silva Bem-vindo".

Outro aspecto importante a ser considerado é o desempenho. Concatenar strings em loops ou em grandes quantidades pode afetar a velocidade do seu aplicativo. Nesses casos, pode ser melhor usar a classe `NSMutableString`, que é projetada para lidar com alterações frequentes em uma string.

## Veja também

- [Documentação oficial do Swift sobre strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial sobre concatenação de strings em Swift](https://www.hackingwithswift.com/read/0/2/concatenating-strings)
- [Exemplos práticos de concatenação de strings em Swift](https://www.codementor.io/@ashishkakkad16/concatenating-strings-with-for-loop-in-swift-5-2r645e7ph)