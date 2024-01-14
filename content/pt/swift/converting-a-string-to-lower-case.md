---
title:                "Swift: Convertendo uma string para minúsculas"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Porquê

Muitas vezes, ao trabalhar com programação em Swift, precisamos lidar com strings de texto. E, às vezes, precisamos transformar essas strings em letras minúsculas. Converter uma string para letras minúsculas pode ser útil para facilitar a comparação de strings ou para garantir que a formatação do texto esteja uniforme.

## Como fazer

Para converter uma string para minúsculas, podemos usar o método `lowercased()` da classe `String` do Swift. Vamos dar uma olhada em um exemplo simples:

```
let nome = "JOÃO"
let nomeMinusc = nome.lowercased()
print(nomeMinusc)
```

Neste código, criamos uma variável `nome` com o valor "JOÃO". Em seguida, usamos o método `lowercased()` para convertê-la para minúsculas e armazená-la na variável `nomeMinusc`. Ao imprimir o valor da variável `nomeMinusc`, obteremos "joão".

Este método também funciona em strings com caracteres acentuados e especiais. Por exemplo:

```
let texto = "AÇÚCAR!"
let textoMinusc = texto.lowercased()
print(textoMinusc)
```

Nesse caso, o resultado impresso será "açúcar!".

## Mergulho profundo

Ao converter uma string para minúsculas, o Swift usa as regras de maiúsculas e minúsculas da linguagem de programação Unicode. Isso significa que os caracteres acentuados e especiais serão convertidos para suas correspondentes letras minúsculas. Além disso, caracteres como o "i" turco (İ) também serão transformados em "i" minúsculo em vez de "ı".

Vale ressaltar que o método `lowercased()` não modifica a string original, mas sim retorna uma nova string com as letras minúsculas. Isso pode ser útil caso você precise manter a string original intacta.

## Veja também

- [Documentação oficial do método lowercased()](https://developer.apple.com/documentation/swift/string/1786160-lowercased)
- [Guia de introdução ao Swift](https://www.swiftbysundell.com/basics/introduction/)
- [Como comparar strings em Swift](https://www.hackingwithswift.com/example-code/strings/how-to-compare-strings-in-swift)