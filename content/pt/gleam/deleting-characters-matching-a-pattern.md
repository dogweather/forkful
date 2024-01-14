---
title:                "Gleam: Excluindo caracteres que correspondem a um padrão."
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que usar a exclusão de caracteres em um padrão?

Você já se deparou com uma situação em que precisa excluir vários caracteres de um texto que seguem um determinado padrão? Com o Gleam, isso pode ser feito de forma rápida e eficiente usando a função "delete_matching" da biblioteca "String". Neste post, vamos te mostrar como fazer isso e aprofundar na função para que você possa entendê-la melhor.

## Como fazer?

Para excluir caracteres de uma string que sigam um padrão, basta usar a função "delete_matching" e passar o texto original e o padrão desejado como argumentos. Vamos ver um exemplo prático:

```Gleam
import String

// Definindo a string original
let texto = "Meu número de telefone é 345-678-910"

// Excluindo os números e o traço
let texto_atualizado = String.delete_matching(texto, "[0-9-]")

IO.print(texto_atualizado) // Saída: "Meu número de telefone é "

```

No exemplo acima, usamos a expressão regular "[0-9-]", que engloba todos os números e o traço. Portanto, todos os caracteres que correspondem a esse padrão foram excluídos da string original. Você pode adaptar a expressão regular de acordo com o seu padrão específico.

## Profundidade da função "delete_matching"

A função "delete_matching" é parte da biblioteca "String" do Gleam, que oferece uma ampla gama de funções úteis para lidar com strings. Ela usa a biblioteca "version" do Gleam para lidar com expressões regulares, o que torna sua aplicação ainda mais flexível.

Além disso, a função "delete_matching" é pura, o que significa que ela não altera a string original, mas sim retorna uma nova string com as alterações aplicadas. Isso é importante para evitar efeitos colaterais indesejados em seu código.

## Veja também

- [Documentação da biblioteca String](https://gleam.run/documentation/stdlib/string.html)
- [Documentação da biblioteca Version](https://gleam.run/documentation/stdlib/version.html)

A exclusão de caracteres em um padrão pode ser uma tarefa comum em programação, por isso é importante entender como fazê-la de forma simples e eficaz. Com a função "delete_matching" do Gleam, você pode facilmente excluir caracteres que seguem um padrão específico em suas strings. Não hesite em experimentar e explorar mais as funcionalidades dessa função e da biblioteca "String" do Gleam.