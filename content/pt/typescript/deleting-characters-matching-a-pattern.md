---
title:                "TypeScript: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Porquê

O TypeScript é uma linguagem de programação cada vez mais popular entre desenvolvedores, devido ao seu forte sistema de tipagem e suporte para orientação a objetos. Porém, como em qualquer linguagem, pode ser necessário lidar com a manipulação de strings. Se você já se perguntou como deletar caracteres em uma string de acordo com um padrão específico, este artigo é para você.

## Como Fazer

Para deletar caracteres em uma string de acordo com um padrão, podemos usar o método `replace()` do TypeScript. Ele substitui uma parte da string por outra, de acordo com as regras definidas. Vamos dar uma olhada em um exemplo:

```TypeScript
let string = "Olá mundo!";
string = string.replace(/[\d\s]/g, "");
console.log(string);
```

Nesse exemplo, usamos o método `replace()` para substituir todos os dígitos e espaços da string por uma string vazia. Como resultado, a saída será "Olámundo!", sem os espaços e números.

Agora, vamos entender como essa expressão regular funciona. A expressão `/[\d\s]/g` significa que queremos substituir todos os caracteres que são números (`\d`) e espaços (`\s`), de forma global (`g`) na string. Então, ao executar o método `replace()`, todos os caracteres correspondentes serão substituídos por uma string vazia.

Existem inúmeras possibilidades de manipulação com o método `replace()` e as expressões regulares, como substituir padrões específicos, caso sensível ou não, entre outros. É importante ter um bom conhecimento dessa ferramenta para efetuar as manipulações desejadas.

## Deep Dive

Manipulação de strings é uma das tarefas mais comuns em programas de computador, e entender como fazer isso corretamente pode economizar muito tempo na hora de escrever código. No TypeScript, o método `replace()` pode ser usado de várias formas, dependendo do que queremos substituir em uma string.

Uma das formas mais eficientes e flexíveis de usar o `replace()` é com expressões regulares. Elas permitem que você especifique um padrão de caracteres a serem substituídos, de forma global ou apenas na primeira ocorrência. Além disso, é possível usar a flag `i` para fazer a substituição sem distinguir maiúsculas de minúsculas, ou a flag `m` para considerar múltiplas linhas.

Outra dica importante é utilizar a capacidade de combinar várias expressões regulares para substituir e transformar uma string. Por exemplo, você pode usar um padrão para substituir um determinado caractere, e depois outro padrão para formatar a string de acordo com o seu desejo.

## Veja Também

Aqui estão alguns links úteis para continuar aprendendo sobre a manipulação de strings e expressões regulares no TypeScript:

- [Documentação oficial sobre expressões regulares no TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutorial sobre expressões regulares no TypeScript](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html)
- [Guia completo sobre manipulação de strings no TypeScript](https://medium.com/webeleon/typescript-strings-8136b3d9e465)