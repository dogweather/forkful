---
title:                "Capitalizando uma string"
html_title:           "Gleam: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?
Capitalizar um string é tornar a primeira letra da sentença em maiúsculo. Programadores fazem isso principalmente para melhorar a apresentação dos dados do usuário final e garantir consistência nos dados de aplicativos.

## Como Fazer:
Criar string capitalizado em Gleam é uma tarefa muito fácil. Vamos passar por um exemplo:

```Gleam
import gleam/string

fn main() {
  let texto = "istö é gleam"
  let capitalized = string.capitalise(texto)

  io.println(capitalized) // Imprime "Istö é gleam"
}
```
Neste exemplo, o método `string.capitalise` é usado para capitalizar a string. Ele transforma o primeiro caractere da string em maiúsculo.

## Mergulhando Profundamente
1. Contexto Histórico: A capitalização de string existe desde o início da programação e é amplamente utilizada em todos os idiomas. Na prática, ajuda a tornar o código mais legível e compreensível.
2. Alternativas: Gleam permite capitalizar uma string de várias maneiras. Além da função `string.capitalise`, também existem funções embutidas em bibliotecas de terceiros que permitem capitalizar uma string.
3. Detalhes de Implementação: No Gleam, a função `string.capitalise` mapeia a primeira letra do string para maiúsculo. Caso a string esteja vazia, o programa retorna a própria string sem nenhuma alteração.

## Veja Também
-Algumas fontes para aprender mais sobre Gleam e suas funções de string:
1. Documentação Oficial do Gleam: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
3. API de string Gleam: [https://hexdocs.pm/gleam_stdlib/gleam/string.html](https://hexdocs.pm/gleam_stdlib/gleam/string.html)