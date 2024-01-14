---
title:    "Gleam: Utilizando expressões regulares"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Porque utilizar expressões regulares em programação

Se você está procurando uma maneira eficiente e poderosa de manipular textos e padrões dentro de seus programas, expressões regulares são a resposta. Essas sequências de caracteres permitem que você encontre e substitua padrões específicos em uma string, economizando tempo e esforço no processamento de grandes quantidades de dados.

# Como usar expressões regulares em Gleam

Em Gleam, você pode usar expressões regulares por meio da biblioteca `regex`. Primeiro, importe a biblioteca em seu arquivo:

```
import regex
```

Em seguida, você pode usar a função `Regex.match` para verificar se um padrão específico é encontrado em uma string. O resultado será um `Result` que pode ser `Ok` se for encontrado ou `Err` se não for encontrado. Aqui está um exemplo:

```
let result = Regex.match(Integer.to_string(2020), "\\d{4}")  // retorna Ok
```

Você também pode usar a função `Regex.replace` para substituir um padrão em uma string por outro valor. Por exemplo, para mudar todos os números em uma string para "x", você poderia usar:

```
let result = Regex.replace("abc123def456", "\\d", "x")  // retorna "abcxxxdefxxx"
```

# Mergulho profundo em expressões regulares

As expressões regulares têm uma sintaxe própria, que pode parecer intimidante no início. No entanto, vale a pena aprender alguns conceitos básicos para aproveitar ao máximo essa ferramenta poderosa. Aqui estão alguns exemplos de padrões comuns que você pode usar:

- `.`: representa qualquer caractere
- `*`: representa zero ou mais ocorrências do caractere anterior
- `+`: representa uma ou mais ocorrências do caractere anterior
- `\d`: representa um dígito
- `\w`: representa um caractere alfanumérico
- `\\`: representa uma barra invertida literal

Você também pode usar colchetes (`[]`) para representar um conjunto de caracteres. Por exemplo, `[A-Z]` significa qualquer letra maiúscula. E você pode usar o operador `|` para combinar diferentes padrões. Por exemplo, `(foo|bar)` significa "foo" ou "bar".

Para saber mais sobre como criar suas próprias expressões regulares e usar suas funções em Gleam, recomenda-se ler a documentação oficial da biblioteca `regex` e praticar com diferentes exemplos.

## Veja também

- [Documentação Gleam de expressões regulares](https://gleam.run/packages/regex/)
- [Tutorial de Expressões Regulares da MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Livro online "Regular Expressions Cookbook"](https://www.regular-expressions.info/cookbook.html)