---
title:    "Swift: Extraindo subcadeias de caracteres"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que extrair substrings em Swift?

Extracting substrings ou, em português, extrair substrings é uma técnica importante no desenvolvimento de aplicativos em Swift. Ela permite que você pegue apenas uma parte específica de uma string ou texto e a utilize em seu código. Isso pode ser útil em muitas situações, como por exemplo formatar um número de telefone ou obter as iniciais de um nome.

# Como fazer isso em Swift?

Para extrair uma substring em Swift, é necessário utilizar o método .substring (ou .prefix para pegar os primeiros caracteres) e indicar o índice inicial e final desejados, dentro de colchetes [ ].

```
// exemplo de substring em Swift
let str = "Olá, meu nome é Maria!"
let nome = str.substring(with: 15..<20)
print(nome) // saída: Maria
```

No exemplo acima, criamos uma string com o nome "Maria" e utilizamos o método .substring para extrair apenas essa parte do texto, indicando os índices 15 e 20 (o primeiro caracter começa com o índice 0). O resultado é impresso no console.

# Mais detalhes sobre extrair substrings em Swift

Além de indicar os índices inicial e final, é possível utilizar outras opções ao extrair substrings em Swift. Por exemplo, é possível obter um determinado número de caracteres a partir de um índice, utilizando o método .prefix.

```
// exemplo de substring com .prefix
let str = "1234567890"
let numeros = str.prefix(5)
print(numeros) // saída: 12345
```

Outra opção é utilizar o método .suffix para extrair uma determinada quantidade de caracteres contando a partir do final da string.

```
// exemplo de substring com .suffix
let str = "ABCDE"
let letras = str.suffix(3)
print(letras) // saída: CDE
```

# Veja também

- [Documentação oficial do Swift sobre substring](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#)
- [Tutorial em português sobre extração de substrings em Swift](https://www.betacode.net/blog/extraindo-partes-de-uma-string-no-swift/)

Agora que você aprendeu como extrair substrings em Swift, pode utilizá-las em suas próximas aplicações para deixar o seu código mais eficiente e fácil de usar. Experimente e veja os resultados por si mesmo!