---
title:                "TypeScript: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Existem muitas situações em programação onde é necessário manipular e extrair partes específicas de uma string. Por exemplo, pode ser necessário obter apenas o primeiro nome de uma pessoa ou extrair uma data específica de um texto longo. Para isso, a extração de substrings é uma técnica poderosa e útil para qualquer programador TypeScript.

## Como fazer

Extrair substrings em TypeScript é bastante simples. Primeiro, precisamos de uma string de exemplo que iremos usar para demonstrar o processo:

```TypeScript
let texto = "Olá, sou um programador TypeScript!"
```

Agora, vamos supor que precisamos extrair apenas a palavra "programador" da string acima. Para fazer isso, podemos usar o método `substring` da classe `String` e fornecer os índices inicial e final da substring desejada. No nosso caso, a palavra "programador" começa no 13º caractere e termina no 22º caractere, então usaremos os índices 12 e 21:

```TypeScript
let substring = texto.substring(12, 21);

console.log(substring); // output: programador
```

E se não soubermos os índices exatos da substring que queremos extrair? Não se preocupe, podemos usar outros métodos, como `indexOf` e `lastIndexOf`, para encontrar a posição dos caracteres que estamos procurando. Vamos supor que queremos extrair a palavra "programador" sem saber seus índices:

```TypeScript
let start = texto.indexOf("programador");
let end = texto.lastIndexOf("programador") + 10; // adicionamos 10 para incluir o último caractere da palavra

let substring = texto.substring(start, end);

console.log(substring); // output: programador
```

## Deep Dive

Agora que entendemos como extrair substrings em TypeScript, é importante saber que existem muitas outras formas de fazer isso. Por exemplo, também podemos usar os métodos `slice`, `substr` e `match` para obter substrings de uma string. Cada um desses métodos tem suas próprias peculiaridades e é importante entender suas diferenças para escolher o mais adequado para cada situação.

Outro ponto importante a ser mencionado é que esses métodos podem ser usados ​​tanto em strings como em Arrays em TypeScript, oferecendo ainda mais flexibilidade e opções para manipulação de dados.

## Veja também

- [Documentação oficial do TypeScript sobre a classe String](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Tutorial sobre extração de substrings em TypeScript](https://codeburst.io/javascript-how-to-get-a-part-of-string-using-substr-slice-and-substring-146ac41abf3a)
- [Explicação profunda sobre os diferentes métodos de extração de substrings](https://humanwhocodes.com/blog/2019/07/extracting-substrings-javascript/)