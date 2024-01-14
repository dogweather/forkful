---
title:                "TypeScript: Removendo caracteres que correspondem a um padrão."
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você pode se deparar com situações em que precisa deletar caracteres que correspondem a um padrão específico em seu código TypeScript. Isso pode ser necessário para limpar ou formatar dados, ou até mesmo para resolver um problema específico em sua lógica de programação. Aprender como fazer isso pode ser útil para melhorar a eficiência e a qualidade do seu código.

## Como fazer

Para deletar caracteres que correspondem a um padrão em TypeScript, você pode usar a função `replace()` combinada com expressões regulares. Por exemplo, se quisermos remover todos os espaços em branco de uma string, podemos usar o seguinte código:

```
const string = "Isso é uma string com espaços em branco"
const novaString = string.replace(/\s/g, "")
console.log(novaString) // "Issoéumastringcomespaçosembranco"
```

O uso da expressão regular `/\s/g` nos permite encontrar todos os espaços em branco na string e, em seguida, substituí-los por uma string vazia, resultando em uma nova versão da string sem espaços em branco.

Você também pode usar outras expressões regulares para atender a diferentes padrões de caracteres que deseja excluir. Por exemplo, se você quiser excluir letras minúsculas de uma string, pode usar `/[a-z]/g`.

## Profundidade

Ao trabalhar com a função `replace()` e expressões regulares, é importante entender como o operador de substituição funciona. Ele substituirá apenas o primeiro caractere que corresponde ao padrão, a menos que você adicione a flag `g` no final da expressão regular para indicar que a substituição deve ser global.

Além disso, você pode usar os caracteres especiais `^` (início da string) e `$` (fim da string) para delimitar o padrão em uma determinada posição na string.

Também é importante notar que o uso de expressões regulares pode afetar o desempenho do seu código, pois elas podem ser processadas de forma mais lenta do que outras operações de string.

## Veja também

- [Documentação TypeScript para função `replace()`](https://www.typescriptlang.org/docs/handbook/utility-types.html#public)
- [Referência de expressões regulares em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)