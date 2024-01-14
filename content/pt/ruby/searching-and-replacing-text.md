---
title:    "Ruby: Buscando e substituindo texto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A busca e substituição de texto é uma habilidade fundamental para qualquer programador Ruby. Ela permite que você encontre e altere rapidamente partes específicas de um texto, tornando seu código mais eficiente e fácil de gerenciar. Se você quer ser um programador eficiente em Ruby, dominar a busca e substituição de texto é essencial.

## Como Fazer

Para realizar a busca e substituição de texto em Ruby, você precisa usar a função `gsub()`. Esta função permite que você especifique o texto a ser encontrado e substituído, bem como o texto de substituição. Veja um exemplo simples:

```Ruby
text = "Este é um texto de exemplo."
puts text.gsub("exemplo", "exercício")
```
Saída: Este é um texto de exercício.

Você também pode usar regex (expressões regulares) para tornar suas substituições de texto mais flexíveis. Por exemplo, se quiser substituir todas as letras maiúsculas por minúsculas em uma string, você pode usar o seguinte código:

```Ruby
text = "Ruby é incrível."
puts text.gsub(/[A-Z]/, &:downcase)
```
Saída: ruby é incrível.

Além disso, você pode usar a função `gsub!()` para substituir diretamente o texto na string original, em vez de criar uma nova string.

## Profundidade

As expressões regulares (regex) são uma parte importante da busca e substituição de texto em Ruby. Elas permitem que você encontre padrões específicos em um texto e substitua-os dinamicamente. Por exemplo, se você quiser substituir todos os números em uma string por ponto e vírgula, você pode usar esta expressão regular: `\d+`, que corresponde a qualquer número de dígitos. Você também pode adicionar modificadores à expressão regular, como `i` para torná-la insensível a maiúsculas e minúsculas e `g` para que ela substitua todas as ocorrências em vez de apenas a primeira.

Outro recurso útil para busca e substituição de texto em Ruby é a função `scan()`, que permite que você encontre todas as ocorrências de um determinado padrão em uma string e as armazene em uma matriz para manipulação posterior.

Certifique-se de dedicar tempo para aprender mais sobre as expressões regulares e suas capacidades. Elas podem ser extremamente poderosas em suas habilidades de busca e substituição de texto.

## Veja também

- [Documentação oficial do Ruby para a função `gsub()`] (https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Ruby: Expressões regulares para iniciantes] (https://www.rubyguides.com/2015/06/ruby-regex/)
- [Tutorial prático de expressões regulares em Ruby] (https://www.rubyguides.com/2015/06/ruby-regex/)