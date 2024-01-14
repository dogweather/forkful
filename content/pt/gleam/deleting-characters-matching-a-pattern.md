---
title:    "Gleam: Excluindo caracteres que correspondem a um padrão"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Blog sobre programação em Gleam para falantes de Português

## Por que

Muitas vezes, em uma string (cadeia de caracteres), podemos nos deparar com a necessidade de remover caracteres que correspondem a um determinado padrão. Isso pode ser útil, por exemplo, para limpar dados ou formatar uma string de acordo com um padrão específico. Neste artigo, vamos explorar como podemos utilizar a linguagem de programação Gleam para realizar essa tarefa de forma eficiente.

## Como Fazer

O primeiro passo para deletar caracteres que correspondem a um padrão em uma string é importar o módulo `String`, que contém funções úteis para manipulação de strings em Gleam.

"```Gleam
import String
```"

Em seguida, podemos utilizar a função `gsub` para substituir todos os caracteres correspondentes a um determinado padrão por uma string vazia.

"```Gleam
String.gsub("Hello World", "[o e]", "")
```

O código acima irá retornar a string "Hll Wrld", pois os caracteres "o" e "e" foram removidos.

Também é possível utilizar expressões regulares na função `gsub`, para que possamos especificar padrões mais complexos. Por exemplo, se quisermos remover todos os caracteres não alfanuméricos de uma string, podemos utilizar a expressão regular `[^a-zA-Z0-9]`.

"```Gleam
String.gsub("Hello World!", "[^a-zA-Z0-9]", "")
```

O resultado será a string "HelloWorld".

## Aprofundando-se

A linguagem de programação Gleam possui suporte nativo para expressões regulares, o que facilita bastante a manipulação de strings. Além disso, a função `gsub` é otimizada para um desempenho eficiente, o que a torna uma ótima opção para a tarefa de deletar caracteres que correspondem a um padrão.

Outra opção para realizar essa tarefa seria utilizar a função `filter`, que filtra uma string com base em uma função fornecida pelo usuário. No entanto, a função `gsub` é mais simples e direta para esse tipo de manipulação.

## Veja Também

- Documentação oficial do módulo `String`: https://gleam.run/modules/string.html
- Tutorial sobre expressões regulares em Gleam: https://gleam.run/articles/regular-expressions.html
- Código fonte do módulo `String`: https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/gleam/String.gleam