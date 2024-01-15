---
title:                "Juntando cadeias de caracteres"
html_title:           "Swift: Juntando cadeias de caracteres"
simple_title:         "Juntando cadeias de caracteres"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Concatenar strings é uma tarefa fundamental para criar aplicativos em Swift. É a forma mais simples de combinar strings e criar mensagens ou informações mais completas para serem exibidas para o usuário.

## Como Fazer
Para concatenar strings em Swift, usamos o operador de adição (+). Ele pode ser usado para unir duas strings ou uma string e uma variável, como mostrado no exemplo abaixo:

```Swift
let nome = "Maria"
let sobrenome = "Silva"
let nomeCompleto = nome + sobrenome // nomeCompleto = "MariaSilva"
```

Também podemos usar o operador de atribuição composto (+=) para adicionar uma nova string a uma já existente, como no exemplo abaixo:

```Swift
var cumprimento = "Olá "
let nome = "Pedro"
cumprimento += nome // cumprimento = "Olá Pedro"
```

Além disso, podemos usar o método `append()` para adicionar uma string ao final de outra, como mostrado a seguir:

```Swift
var nome = "João"
let sobrenome = "Silva"
nome.append(sobrenome) // nome = "JoãoSilva"
```

## Mergulho Profundo
Existem algumas coisas a ter em mente ao concatenar strings em Swift. Primeiro, é importante saber que o operador de adição só pode ser usado entre duas strings ou uma string e uma variável do tipo String. Se tentarmos usar o operador com tipos diferentes, receberemos um erro.

Além disso, ao usar o método `append()`, precisamos estar atentos ao tipo de variável que estamos adicionando. Se adicionarmos uma string a uma variável do tipo Character, por exemplo, receberemos um erro. Por isso, é importante garantir que as variáveis utilizadas estejam do mesmo tipo antes de concatená-las.

Uma boa prática é usar o método `String(format: )` para juntar múltiplas strings em um único formato, como mostrado no exemplo abaixo:

```Swift
let nome = "Ana"
let sobrenome = "Souza"
let idade = 25
let mensagem = String(format: "Meu nome é %@ %@ e tenho %d anos.", nome, sobrenome, idade) 
// mensagem = "Meu nome é Ana Souza e tenho 25 anos."
```

Por fim, é importante lembrar que ao concatenar strings, podemos usar qualquer caractere ou símbolo, como espaços, letras com acentos e até emojis, desde que estejam entre as aspas.

## Veja Também
- Documentação oficial Apple sobre Strings em Swift: https://developer.apple.com/documentation/swift/string
- Tutorial sobre como trabalhar com strings em Swift: https://www.appcoda.com/swift-string/
- Vídeo explicativo sobre concatenar strings em Swift: https://www.youtube.com/watch?v=0DAkI30B0Jc