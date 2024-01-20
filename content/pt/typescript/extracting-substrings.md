---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Trabalhando com substrings em TypeScript: um guia descomplicado

---

## O Que & Por Quê?

Extrair substrings envolve a recuperação de uma porção de uma string existente. Programadores fazem isso regularmente sempre que necessitam de um segmento específico de uma string, ao invés do texto completo.

## Como Fazer:

TypeScript permite a extração de substrings de várias maneiras. Aqui estão as mais comuns - utilizando os métodos `substring()`,  `slice()` e `substr()`.

```TypeScript
let texto: string = "Ola, mundo TypeScript!";

// Utilizando substring()
let substringRes = texto.substring(5, 11);
console.log(substringRes);  // Saída: mundo

// Utilizando slice()
let sliceRes = texto.slice(5, 11);
console.log(sliceRes);  // Saída: mundo

// Utilizando substr()
let substrRes = texto.substr(5, 5);
console.log(substrRes);  // Saída: mundo
```

Em todos os três exemplos acima, estamos extraindo a substring "mundo" da string original.

## Análise Detalhada:

Extrair substrings é uma prática comum em programação e tem sido uma parte importante das linguagens de programação desde suas concepções. TypeScript, sendo um superconjunto de JavaScript, incorpora o mesmo conjunto de métodos para lidar com substrings.

Em relação às alternativas, além dos métodos já mencionados, pode-se usar expressões regulares ou até bibliotecas externas, caso a lógica necessária seja muito complexa.

Os detalhes da implementação podem variar dependendo do método escolhido. De um modo geral, `substring()` e `slice()` são muito parecidos, porém a principal diferença entre eles está em como lidam com argumentos negativos. `substring()` simplesmente os transforma em 0, enquanto `slice()` os trata como referência contada a partir do fim da string. Já o `substr()`, embora seja similar, sua segunda variável indica o número de caracteres para extrair, não a posição de fim.

## Veja Também:

- [Documentação de strings do TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)