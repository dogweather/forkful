---
title:    "TypeScript: Maiúsculas em uma cadeia de caracteres."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em TypeScript?

Capitalizar uma string é uma tarefa comum na programação, especialmente quando se lida com entradas de usuário ou manipulação de dados. Em TypeScript, capitalizar uma string pode ser útil para padronizar a formatação de informações ou para fins estéticos. Neste artigo, discutiremos como capitalizar uma string em TypeScript e como isso pode ser útil em seus projetos.

## Como fazer: capitalizando uma string em TypeScript

Para capitalizar uma string em TypeScript, podemos usar o método `toUpperCase()` junto com a função `charAt()` e `substring()`. Veja um exemplo abaixo:

```TypeScript
let texto = "exemplo de texto";
texto = texto.charAt(0).toUpperCase() + texto.substring(1);

console.log(texto); // Saída: "Exemplo de texto"
```

Neste exemplo, usamos a função `charAt()` para obter o primeiro caractere da string e, em seguida, o método `toUpperCase()` para transformá-lo em maiúsculo. Em seguida, usamos a função `substring()` para pegar o restante da string, a partir do segundo caractere, e juntamos com o primeiro caractere maiúsculo. Assim, temos uma string totalmente capitalizada.

Outra forma de capitalizar uma string é usando a biblioteca `lodash`, que possui o método `capitalize()`. Veja um exemplo:

```TypeScript
import capitalize from 'lodash/capitalize';

let texto = "uma outra forma de capitalizar";
texto = capitalize(texto);

console.log(texto); // Saída: "Uma outra forma de capitalizar"
```

Além disso, podemos criar uma função que recebe uma string como parâmetro e retorna a string capitalizada. Isso pode ser útil em casos em que precisamos capitalizar várias strings em diferentes momentos do nosso código. Veja um exemplo:

```TypeScript
function capitalizarString(texto: string) {
  return texto.charAt(0).toUpperCase() + texto.substring(1);
}

let nome = "joão";
let sobrenome = "silva";

console.log(capitalizarString(nome)); // Saída: "João"
console.log(capitalizarString(sobrenome)); // Saída: "Silva"
```

Com esses exemplos, fica claro como podemos capitalizar uma string em TypeScript, seja utilizando métodos nativos ou bibliotecas externas.

## Deep Dive: explorando mais sobre a capitalização de strings

Também é importante mencionar que podemos transformar uma string inteira em maiúsculo ou minúsculo usando os métodos `toUpperCase()` e `toLowerCase()`, respectivamente. Além disso, é possível capitalizar cada palavra de uma string usando o método `replace()` junto com uma expressão regular.

Também é interessante notar que, ao capitalizar uma string em TypeScript, estamos trabalhando apenas com os caracteres ASCII. Portanto, se tivermos uma string com caracteres especiais ou acentuação, é importante considerar isso em nosso código.

Outra dica útil é utilizar a função `trim()` para remover possíveis espaços em branco antes e depois da string, garantindo assim uma capitalização correta.

## Veja também

- Documentação oficial sobre as funções e métodos usados neste artigo: https://www.typescriptlang.org/docs
- Documentação sobre a biblioteca `lodash`: https://lodash.com/docs
- Mais informações sobre expressões regulares: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions