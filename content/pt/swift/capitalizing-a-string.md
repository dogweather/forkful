---
title:                "Swift: Capitalizando uma string"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que Capitalizar uma String em Swift?

Capitalizar uma string é uma tarefa bastante comum em programação, pois muitas vezes é necessário padronizar o formato do texto em um aplicativo ou site. Ao capitalizar uma string, estamos deixando todas as letras iniciais das palavras como maiúsculas e as demais como minúsculas. Isso pode ser útil para melhorar a estética de um texto, facilitar a leitura e até mesmo para realizar comparações entre strings. Neste artigo, vamos explorar como capitalizar uma string em Swift e entender um pouco mais sobre essa funcionalidade.

## Como Capitalizar uma String em Swift?

Para capitalizar uma string em Swift, utilizamos o método `capitalized` da classe `String`. Ele retorna uma nova string com todas as letras iniciais de cada palavra convertidas para maiúsculas. Veja o exemplo abaixo:

```Swift
let texto = "programar em swift é muito divertido"
print(texto.capitalized)

//Saída: Programar Em Swift É Muito Divertido
```

Além disso, podemos utilizar o método `uppercased` para transformar todas as letras da string em maiúsculas ou `lowercased` para transformá-las em minúsculas. Veja um exemplo abaixo:

```Swift
let texto = "Olá, Mundo!"
print(texto.uppercased())
print(texto.lowercased())

//Saída: OLÁ, MUNDO!
//Saída: olá, mundo!
```

Caso precisemos capitalizar apenas a primeira letra de uma string, podemos utilizar o método `capitalizedFirstLetter` abaixo, que utiliza a função `prefix` e `suffix` para retornar um substring com a primeira letra em maiúscula e o restante da string em minúscula:

```Swift
extension String {
    var capitalizedFirstLetter: String {
        guard let firstLetter = self.first else { return "" }
        return firstLetter.uppercased() + self.dropFirst().lowercased()
    }
}

let texto = "programar em swift é muito divertido"
print(texto.capitalizedFirstLetter)

//Saída: Programar em swift é muito divertido
```

## Deep Dive

Além dos métodos mencionados anteriormente, também é possível capitalizar apenas a primeira letra de cada palavra em uma string utilizando a classe `StringTransform`.

```Swift
let texto = "programar em swift é muito divertido"
let capitalizarPrimeiraLetra = StringTransform(rawValue: "Any-Title")!
let textoFormatado = texto.applyingTransform(capitalizarPrimeiraLetra, reverse: false)
print(textoFormatado!)

//Saída: Programar Em Swift É Muito Divertido
```

Também é importante mencionar que a função `capitalized` pode não produzir o resultado esperado em alguns casos específicos, como quando a string contém caracteres especiais ou emojis. Por isso, é importante entender bem as limitações dessa funcionalidade ao utilizá-la em seus projetos.

## Veja também

- [Documentação da Apple sobre a classe String](https://developer.apple.com/documentation/swift/string)
- [Blog post sobre como capitalizar uma string em Swift](https://medium.com/@sachinkirank/capitalizing-and-more-9f98d0f54765)
- [Tutorial sobre manipulação de strings em Swift](https://www.hackingwithswift.com/articles/78/how-to-manipulate-strings-in-swift)