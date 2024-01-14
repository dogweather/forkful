---
title:                "TypeScript: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em TypeScript?

Concatenar strings é uma tarefa comum em programação, especialmente em TypeScript. É útil quando se deseja combinar várias strings em uma única e, assim, criar mensagens dinâmicas ou construir URLs.

## Como fazer isso em TypeScript

Em TypeScript, a concatenação de strings pode ser feita de várias maneiras. Uma delas é usando o operador `+`, como mostrado abaixo:

```TypeScript
let nome = 'Maria';
let saudacao = 'Olá';

console.log(nome + ', ' + saudacao); // Saída: Maria, Olá
```

Outra opção é usar a função `concat()`:

```TypeScript
let sobrenome = 'Silva';

console.log(nome.concat(' ', sobrenome)); // Saída: Maria Silva
```

Além disso, é possível usar template literals, que permitem a interpolação de variáveis em strings:

```TypeScript
console.log(`${nome} ${sobrenome}`); // Saída: Maria Silva
```

## Profundidade sobre a concatenação de strings

Vale ressaltar que, ao usar o operador `+` para concatenar strings, o TypeScript realiza uma conversão implícita para o tipo `string` caso algum dos valores seja numérico. Por exemplo:

```TypeScript
let idade = 25;
let frase = 'Eu tenho ' + idade + ' anos';

console.log(frase); // Saída: Eu tenho 25 anos
```

No entanto, essa conversão implícita pode levar a resultados indesejados e ainda causar erros. Portanto, é importante estar ciente disso e, se necessário, converter explicitamente os valores para string antes da concatenação.

Outra dica importante é que, ao trabalhar com grandes quantidades de strings, é mais eficiente concatená-las usando o método `join()` em um array. Isso é especialmente útil se as strings já estiverem em um array ou se forem geradas em um loop. Veja o exemplo:

```TypeScript
let cores = ['vermelho', 'azul', 'amarelo'];
let texto = cores.join(', ');

console.log(texto); // Saída: vermelho, azul, amarelo
```

## Veja também

- [Documentação do TypeScript sobre string concatenation](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-literal-types)
- [Artigo do site codeburst sobre a concatenação de strings em TypeScript](https://codeburst.io/how-to-concatenate-strings-in-typescript-ed706e2f2c7b)
- [Vídeo do canal Codevolution no YouTube sobre string concatenation em TypeScript](https://www.youtube.com/watch?v=hvj_PZv5l8E)