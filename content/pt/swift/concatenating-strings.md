---
title:    "Swift: Concatenando strings"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que concatenar strings é importante?

Concatenar strings, ou seja, juntar duas ou mais sequências de caracteres em uma só, é um conceito importante na programação Swift. Isso permite que os desenvolvedores criem strings dinamicamente e manipulem seu conteúdo de forma eficiente. Além disso, é uma habilidade essencial para criar strings personalizadas para serem exibidas em interfaces de usuário, mensagens de alerta e muito mais.

## Como fazer:

Para concatenar strings em Swift, é necessário usar o operador "+" ou o método "append()". Veja abaixo um exemplo de como usar essas duas opções:

```Swift
let nome = "João"
let sobrenome = "Silva"

// Usando o operador "+"
let nomeCompleto = nome + " " + sobrenome
// saída: João Silva

// Usando o método "append()"
var mensagem = "Bem-vindo "
mensagem.append(nome)
mensagem.append(".")
// saída: Bem-vindo João.
```

Ao concatenar strings, é importante prestar atenção nos tipos de dados. Se um dos elementos for outro tipo de dado, como um número inteiro, ele precisará ser convertido para string antes da concatenação. Isso pode ser feito usando o método "String()" ou interpolando a variável dentro de uma string usando o operador "\\". Por exemplo:

```Swift
let idade = 20

// Usando o método "String()"
let mensagem = "Eu tenho " + String(idade) + " anos."
// saída: Eu tenho 20 anos.

// Usando interpolação
let mensagem = "Eu tenho \(idade) anos."
// saída: Eu tenho 20 anos.
```

## Deep Dive:

Ao concatenar strings, é importante também entender a diferença entre o operador "+" e o método "append()". O operador "+" é mais eficiente do que o método "append()", pois ele cria uma nova string que contém os valores combinados. Já o método "append()" adiciona o valor à string existente, o que pode ser um pouco mais lento em termos de desempenho.

Além disso, é importante lembrar que a ordem dos valores importa ao usar o operador "+". Por exemplo, em "nome + sobrenome", a string resultante terá o primeiro nome seguido pelo sobrenome. Já em "sobrenome + nome", a ordem será invertida.

## Veja também:

- [Documentação oficial Apple sobre strings em Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial de concatenar strings em Swift](https://www.tutorialspoint.com/swift-programming/swift_concatenating_strings.htm)
- [Vídeo explicando a diferença entre o operador "+" e o método "append()"](https://www.youtube.com/watch?v=jYjJpX0oWks&ab_channel=Codecademy)