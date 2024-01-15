---
title:                "Utilizando expressões regulares"
html_title:           "Gleam: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, quando estamos escrevendo um programa, precisamos pesquisar e manipular padrões de texto específicos. É aí que entram as expressões regulares, uma ferramenta poderosa para trabalhar com strings. Elas permitem que você encontre e substitua padrões de uma forma mais eficiente do que simplesmente usando funções padrão de strings.

## Como Fazer

Para usar expressões regulares no Gleam, você precisa importar a biblioteca "re". Você pode fazer isso adicionando `import re` no início do seu código. Agora, vamos ver como encontrar um padrão específico em uma string e substituí-lo por outro.

```
Gleam import re

fn main() {
  let regex = `(?i)apple` // Define o padrão que queremos encontrar
  let string = "Eu amo Apple." // Declara a string que será pesquisada
  let novo_string = re.replace(string, regex, "abacate") // Substitui "Apple" por "abacate"
}
```

Neste exemplo, usamos o sinal `(?i)` para indicar que a busca deve ser feita sem diferenciação entre maiúsculas e minúsculas. Você pode usar outros sinais para especificar diferentes tipos de padrões, como `+` para indicar uma ou mais ocorrências e `*` para indicar nenhuma ou mais ocorrências. Para mais detalhes sobre como criar e usar expressões regulares no Gleam, você pode conferir a documentação oficial da biblioteca "re".

## Mergulho Profundo

Uma das vantagens de usar expressões regulares é que elas podem economizar tempo e linhas de código. Por exemplo, ao invés de criar várias condições com operadores de strings, você pode usar apenas uma expressão regular para encontrar e manipular padrões específicos. Além disso, as expressões regulares são amplamente utilizadas em diversas linguagens de programação, o que significa que aprendê-las no Gleam pode te ajudar a entender e trabalhar com essas ferramentas em outras linguagens também.

## Veja Também

- Documentação oficial da biblioteca "re": https://gleam.run/modules/re.html
- Tutorial sobre expressões regulares no Gleam: https://gleam.run/book/tour-regular-expressions.html