---
title:                "Usando expressões regulares"
date:                  2024-02-03T19:18:35.633087-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Expressões regulares, ou regex, são uma ferramenta poderosa de correspondência de padrões e busca na programação. Os programadores utilizam regex para tarefas como validar entradas de usuários, buscar textos ou manipular strings porque é eficiente e versátil.

## Como fazer:

Vamos mergulhar no TypeScript e ver como o regex é usado para tarefas comuns.

```TypeScript
// Definir um padrão regex para um endereço de e-mail
const emailPattern = /\S+@\S+\.\S+/;

// Testar se uma string corresponde ao padrão de e-mail
const email = "usuario@example.com";
console.log(emailPattern.test(email)); // Saída: true

// Encontrar e substituir dígitos em uma string
const replaceDigits = "Item 25 custa $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Saída: "Item # custa $#"

// Extraindo partes específicas de uma string usando grupos de captura
const data = "Abril 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, mês, dia, ano] = datePattern.exec(data) || [];
console.log(mês, dia, ano); // Saída: "Abril" "10" "2021"
```

## Aprofundamento

Na década de 1950, o matemático Stephen Kleene descreveu expressões regulares como um modelo para representar linguagens regulares, que mais tarde se tornaram essenciais em ciência da computação. Avançando no tempo, regex é onipresente na programação para lidar com texto.

Embora regex seja um canivete suíço para operações com string, não está sem alternativas. Dependendo da complexidade da tarefa, às vezes métodos de string como `includes()`, `startsWith()`, `endsWith()`, ou até mesmo análise com uma biblioteca podem ser melhores. Por exemplo, analisar uma string JSON complexa usando regex pode ser um pesadelo—use um analisador JSON em vez disso.

Quanto à implementação, regex em JavaScript e TypeScript é baseada na especificação da linguagem ECMAScript. Por debaixo do capô, os motores usam máquinas de estado para combinar padrões de forma eficiente. Vale ressaltar que operações regex podem se tornar caras em termos de desempenho, especialmente com padrões mal escritos—cuidado com o "catastrophic backtracking".

## Veja Também

- MDN Web Docs sobre Expressões Regulares: [MDN Expressões Regulares](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: Uma ferramenta para testar e depurar padrões regex [Regex101](https://regex101.com/)
- Livro "Mastering Regular Expressions" para um entendimento aprofundado: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
