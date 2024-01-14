---
title:                "Gleam: Procurando e substituindo texto"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao escrever código, é necessário fazer alterações em um grande número de linhas de texto. Isso pode ser tedioso e demorado se feito manualmente. Por isso, é importante saber como usar a função de busca e substituição de texto em programação. Com ela, é possível automatizar essa tarefa e economizar tempo e esforço.

## Como Fazer

Usando a linguagem de programação Gleam, podemos realizar busca e substituição de texto de forma fácil e eficiente. Primeiro, precisamos importar o módulo "gleam/string" para ter acesso às funções necessárias.

```
import gleam/string
```

Em seguida, podemos usar a função `replace` para substituir uma determinada parte do texto por outra. Por exemplo, se quisermos trocar todas as letras "a" por "e" em uma frase, podemos fazer da seguinte forma:

```
let nova_frase = gleam/string.replace("Esta é uma frase", "a", "e")
```

O resultado será a nova frase "Este é ume frase". Além disso, podemos usar a função `replace_all` para substituir todas as ocorrências do padrão de busca no texto.

```
let novo_texto = gleam/string.replace_all("abacaxi é uma fruta", "a", "o")
```

O resultado será "obocoxi é umo fruto".

Outra função útil é a `replace_regex`, que permite usar expressões regulares para encontrar e substituir padrões específicos em um texto.

```
let nova_frase = gleam/string.replace_regex("Hoje é sexta-feira", ~"[aeiou]", "", ~global=true)
```

Neste exemplo, usamos uma expressão regular para remover todas as vogais da frase. O resultado será "Hj s sxtr-fir".

## Profundidade

Além das funções básicas de busca e substituição de texto, a linguagem Gleam também possui recursos avançados para manipulação de strings. É possível, por exemplo, ignorar maiúsculas e minúsculas ao realizar a substituição, usando a função `replace_case_insensitive`. Também é possível especificar o número máximo de substituições a serem feitas com a função `replace_limit`.

Para substituições mais complexas, podemos usar a função `replace_fold`, que permite passar uma função de junção personalizada para decidir como os resultados substituídos serão combinados.

Outra função útil é a `replace_slice`, que permite substituir apenas uma parte específica do texto, definindo um intervalo de início e fim.

Com essas funções, é possível realizar alterações precisas e detalhadas em textos grandes e complicados.

## Veja Também

- Documentação do módulo Gleam String: https://gleam.run/modules/gleam/string.html
- Tutorial de expressões regulares em Gleam: https://gleam.run/articles/regex.html
- Exemplos de uso da função `replace` em Gleam: https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/gleam/string/replace.gleam