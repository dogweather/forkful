---
title:    "TypeScript: Encontrando o comprimento de uma string"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação e é importante entender como fazê-lo em TypeScript. Saber o comprimento de uma string pode ser útil para várias tarefas, como validar entrada do usuário, formatar saída de dados ou manipular strings em geral.

## Como Fazer

Para encontrar o comprimento de uma string em TypeScript, usamos o método `length` que é herdado da classe `String`. Ele retorna o número de caracteres na string e pode ser usado tanto em strings fixas quanto em variáveis ​​de string.

```TypeScript
//Definindo uma string fixa
let frase: string = "Olá mundo!"

//Encontrando o comprimento da string
let comprimento: number = frase.length

console.log(comprimento) //Output: 10
```

Também podemos usar o método `length` em uma variável de string para encontrar o comprimento da string armazenada nela.

```TypeScript
//Definindo uma variável string
let meuNome: string = "Maria"

//Encontrando o comprimento da string armazenada na variável
let tamanho: number = meuNome.length

console.log(tamanho) //Output: 5
```

Também é importante lembrar que, na contagem de caracteres, espaços em branco e outros caracteres especiais também são considerados.

## Deep Dive

O método `length` é na verdade apenas uma propriedade de leitura, o que significa que seu valor não pode ser redefinido. Ele funciona em todos os tipos de string, incluindo strings fixas, string de templates e valores de string de objetos.

Também é interessante notar que o método `length` não leva nenhum parâmetro, ele simplesmente retorna o comprimento da string em uma única unidade. Além disso, ele retorna um valor do tipo `number`, pois esse é o tipo de dado utilizado para representar o comprimento de uma string em TypeScript.

## Veja Também

- [Documentação do método `length` no site oficial do TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#length)
- [Tutorial sobre manipulação de strings em TypeScript](https://dev.to/ahsanayan/typescript-string-operations-c42)
- [Exemplos práticos de uso do método `length` em TypeScript](https://www.javatpoint.com/typescript-string-length)