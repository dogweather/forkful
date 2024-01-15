---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Javascript: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

A exclusão de caracteres que correspondem a um determinado padrão é uma tarefa comum em programação e pode ser útil para remover informações indesejadas ou formatar strings. Isso pode ser útil, por exemplo, para limpar dados importados de uma fonte externa ou para preparar uma string para ser usada em outra parte do código. 

## Como Fazer

Para excluir caracteres que correspondem a um padrão em uma string, podemos usar os métodos `replace()` ou `replaceAll()` do JavaScript. Ambos aceitam dois argumentos: o primeiro é o padrão a ser encontrado e o segundo é o que será substituído no lugar do padrão.

```
let string = "hoje é um ótimo dia para programar";

string = string.replace("ó", "o");

console.log(string);
// output: hoje é um otimo dia para programar

string = string.replaceAll("h", "");

console.log(string);
// output: oje é um otimo dia para programar
```

Note que o método `replace()` só substitui a primeira ocorrência do padrão, enquanto o `replaceAll()` substitui todas as ocorrências. Além disso, podemos usar expressões regulares no primeiro argumento dos métodos para buscar padrões mais complexos. 

```
let string = "123-456-7890";

string = string.replaceAll(/\d/g, "");

console.log(string)
// output: ---
```

Neste exemplo, estamos usando a expressão regular `/\d/g`, que significa "encontrar todos os dígitos" e substituí-los por uma string vazia. Dessa forma, removemos todos os números da string original.

## Aprofundando

A exclusão de caracteres que correspondem a um padrão é uma técnica muito útil em programação, mas é importante ter cuidado ao usá-la. Se o padrão for muito abrangente, podemos acabar excluindo mais do que pretendíamos, resultando em erros ou comportamentos inesperados. Portanto, é importante ter uma boa compreensão da documentação dos métodos `replace()` e `replaceAll()` antes de usá-los.

Além disso, devemos ter em mente que tanto o `replace()` quanto o `replaceAll()` retornam uma nova string, não modificam a original. Por isso, é necessário atribuir o resultado a uma variável se quisermos salvar a string após a exclusão dos caracteres.

## Veja também

- [Documentação do método `replace()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Documentação do método `replaceAll()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)