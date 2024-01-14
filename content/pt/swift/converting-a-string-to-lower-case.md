---
title:    "Swift: Convertendo uma string para minúsculas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Em alguns casos, é necessário converter uma string para letras minúsculas para garantir que a mesma seja processada corretamente, independentemente de como ela foi digitada ou inserida. Por exemplo, em sistemas de login, é comum converter as senhas dos usuários para letras minúsculas antes de compará-las, para garantir que não haja erros de digitação.

## Como fazer?

Existem várias maneiras de converter uma string para letras minúsculas em Swift. A forma mais simples é utilizando o método `lowercased()` da classe `String`. Veja um exemplo:

```Swift
let texto = "Exemplo de String"
let textoMinisculo = texto.lowercased()
print(textoMinisculo)
// Saída: exemplo de string
```

Outra opção é utilizar o operador `+=` para adicionar um modificador de string `lowercased` ao final de uma string existente. Veja:

```Swift
var texto = "Exemplo de String"
texto += .lowercased
print(texto)
// Saída: exemplo de string
```

Além disso, também é possível usar a função `map` para mapear cada caractere da string para sua versão em letras minúsculas. Veja:

```Swift
let texto = "Exemplo de String"
let textoMinisculo = texto.map { $0.lowercased() }
print(textoMinisculo)
// Saída: exemplo de string
```

## Profundando um pouco mais

Ao converter uma string para letras minúsculas, é importante ter em mente que o resultado pode variar dependendo do idioma e do sistema de codificação utilizado. Por exemplo, em maioria dos idiomas, o caractere "I" maiúsculo é convertido para "i" minúsculo, mas em alguns idiomas, como o turco, isso não acontece. Portanto, é importante sempre testar e garantir que a conversão esteja correta para o seu caso específico.

Outro ponto a ser considerado é que o método `lowercased()` não altera a string original, mas sim retorna uma nova string com as letras minúsculas. Se desejar alterar a string original, é preciso atribuir o resultado a ela mesma, como no exemplo a seguir:

```Swift
var texto = "Exemplo de String"
texto = texto.lowercased()
print(texto)
// Saída: exemplo de string
```

## Veja também

- [Documentação oficial da Apple sobre a classe String](https://developer.apple.com/documentation/foundation/string)
- [Tutorial sobre strings em Swift no site Apple Developer](https://developer.apple.com/swift/blog/?id=34)
- [Exemplo de conversão de um texto para letras minúsculas em Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-lowercase)
- [Discussão sobre a conversão de strings em diferentes idiomas em fórum da Apple Developer](https://developer.apple.com/forums/thread/21797)