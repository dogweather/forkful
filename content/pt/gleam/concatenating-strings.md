---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Concatenar "strings" é o processo de combinar duas ou mais cadeias de caracteres em uma única string. Programadores fazem isso para manipular ou formatar texto de maneira eficiente e clara.

## Como Fazer:

No Gleam, a concatenação de strings é realizada utilizando a função `++`. Aqui estão alguns exemplos:

```Gleam

let bem_vindo = "Olá, " ++ "mundo!"
Io.println(bem_vindo) // Imprime: Olá, mundo!

let nome = "João"
let saudacao = "Bom dia, " ++ nome
Io.println(saudacao)  // Imprime: Bom dia, João

```

Nesse código, as strings "Olá, " e "mundo!" são combinadas para formar "Olá, mundo!". Da mesma forma, combinamos "Bom dia, " e a variável `nome` para cumprimentar o usuário.

## Mergulho Profundo:

Historicamente, algumas linguagens de programação, como C, implementavam a concatenação de strings manualmente com loops e ponteiros, o que requeria tempo e podia gerar erros. Felizmente, a maioria das linguagens modernas, como Gleam, fazem isso automaticamente com operadores ou funções dedicados.

Como uma alternativa à concatenação, Gleam também suporta "string interpolation", que pode ser uma solução mais prática para strings complexas. 

O Gleam implementa a concatenação de strings utilizando uma abordagem otimizada, ligando as strings ao invés de copiá-las. Isso significa que a operação é bastante rápida e eficiente.

## Veja Também:

Para mais exemplos e detalhes sobre manipulação de strings em Gleam, confira a documentação oficial ([Gleam Strings Documentation](https://hexdocs.pm/gleam_stdlib/gleam/string.html)) e o fórum de discussão da comunidade ([Gleam Forum](https://gleam.run/community/)).