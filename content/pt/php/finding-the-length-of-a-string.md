---
title:                "Encontrando o comprimento de uma string."
html_title:           "PHP: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que é importante?

Encontrar o comprimento de uma string é uma tarefa comum em programação, pois permite que os programadores saibam quantos caracteres uma string contém. Isso pode ser útil para diversas tarefas, como validação de entrada de dados ou manipulação de strings em algoritmos.

## Como fazer:

O PHP possui uma função integrada, `strlen()`, que pode ser usada para encontrar o comprimento de uma string. Veja um exemplo de como usá-la:

```PHP
$texto = "Olá, mundo!";
echo strlen($texto);
// Saída: 12
```

É importante notar que a função contará todos os caracteres presentes na string, incluindo espaços e pontuação.

## Mais detalhes:

Na história da programação, encontrar o comprimento de uma string era uma tarefa mais complexa, muitas vezes exigindo o uso de funções personalizadas e cálculos matemáticos. No entanto, com o avanço das linguagens de programação, funções como `strlen()` tornaram essa tarefa muito mais simples.

Além disso, algumas linguagens de programação, como Python, possuem uma função semelhante chamada `len()`, que pode ser usada para encontrar o comprimento de uma string.

A implementação da função `strlen()` no PHP é eficiente e otimizada, utilizando algoritmos de baixo nível para contar os caracteres de forma rápida e precisa.

## Veja também:

- Documentação oficial do PHP sobre a função `strlen()`: https://www.php.net/manual/pt_BR/function.strlen.php
- Vídeo tutorial sobre como usar a função `strlen()` no PHP: https://www.youtube.com/watch?v=dQsIXL1h6VI 
- Outras funções úteis para manipulação de strings no PHP: https://www.php.net/manual/pt_BR/ref.strings.php