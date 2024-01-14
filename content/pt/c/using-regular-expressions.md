---
title:    "C: Usando expressões regulares"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares?

As Expressões Regulares são uma ferramenta poderosa que facilitam a busca e a manipulação de texto em programas de computador. Elas permitem realizar operações complexas de forma eficiente e simplificada, tornando o código mais legível e fácil de ser mantido.

## Como utilizar Expressões Regulares em C

Para utilizar Expressões Regulares em um programa em C, é necessário incluir a biblioteca "regex.h". Nesta biblioteca, encontram-se diversas funções úteis para trabalhar com expressões regulares. Abaixo, segue um exemplo de como utilizar a função "regcomp" para compilar uma expressão regular:

```C
#include <regex.h>

regex_t regex;
char* pattern = "hello";
int status = regcomp(&regex, pattern, 0);

if (status == 0) {
	printf("Expressão regular compilada com sucesso!\n");
} else {
	printf("Erro ao compilar expressão regular.\n");
}
```

O código acima compila a expressão regular "hello" e guarda o resultado na variável "regex". Além disso, verifica o status da compilação e exibe uma mensagem correspondente.

## Mergulhando mais fundo nas Expressões Regulares

Existem diversas formas de utilizar Expressões Regulares em C, desde a verificação de um padrão simples até a substituição de substrings em um texto. É importante conhecer as diferentes funções disponíveis na biblioteca "regex.h" e seus respectivos parâmetros para aproveitar ao máximo essa ferramenta. Aqui estão alguns links úteis para você se aprofundar nas Expressões Regulares em C:

- [Documentação oficial da biblioteca "regex.h"](http://pubs.opengroup.org/onlinepubs/009695399/functions/regcomp.html)
- [Tutorial completo sobre Expressões Regulares em C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Curso online gratuito sobre Expressões Regulares em C](https://www.udemy.com/expressoes-regulares-em-c/)

## Veja também

- [Introdução às Expressões Regulares em C](https://www.meuprograma.com.br/expressoes-regulares-em-c)
- [10 exemplos práticos de Expressões Regulares em C](https://www.devmedia.com.br/expressoes-regulares-c/29392)