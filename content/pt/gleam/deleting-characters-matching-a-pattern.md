---
title:    "Gleam: Excluindo caracteres que correspondem a um padrão"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, em nosso código Gleam, podemos nos deparar com caracteres indesejados que não fazem parte do padrão que estamos buscando. Talvez seja um espaço em branco ou um caractere especial que não deveria estar lá. Nesses casos, podemos usar a função `delete_chars_matching` para remover esses caracteres e manter apenas o que precisamos.

## Como Fazer

Para usar a função `delete_chars_matching`, precisamos passar dois argumentos: a string que queremos verificar e o padrão de caracteres que queremos remover. Por exemplo, se quisermos remover todos os espaços em branco de uma string, podemos fazer o seguinte:

```Gleam
let string = "Este é um exemplo de string com espaços em branco."
let nova_string = delete_chars_matching(string, " ")
```

O resultado será a nova string sem os espaços em branco: "Esteéumexemplodestringcomespaçosembranco."

Podemos também usar expressões regulares para especificar padrões mais complexos. Por exemplo, se quisermos remover todos os dígitos de uma string, podemos fazer o seguinte:

```Gleam
let string = "12345abcde"
let nova_string = delete_chars_matching(string, "[0-9]+")
```

Neste caso, a nova string será "abcde", pois todos os dígitos foram removidos.

## Profundidade

Por trás dos bastidores, a função `delete_chars_matching` faz uso da função `delete_chars_at_indices`, que recebe uma string e uma lista de índices e remove os caracteres correspondentes aos índices da string. A função `delete_chars_matching` simplesmente usa expressões regulares para gerar a lista de índices a serem removidos.

Além disso, é importante notar que a função `delete_chars_matching` retorna uma nova string e não altera a string original. Isso significa que precisamos armazenar o resultado em uma nova variável para usá-lo posteriormente.

## Veja Também

- [Documentação oficial do Gleam sobre `delete_chars_matching`](https://gleam.run/std/delete_chars_matching.html)
- [Tutorial sobre expressões regulares em Gleam](https://dev.to/gleam-lang/regular-expressions-in-gleam-3p3g)
- [Mais recursos sobre o padrão](https://www.gleam.run/learn/patterns.html)