---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

A busca e substituição de texto são operações comuns na programação para encontrar padrões específicos em uma string e substituí-los por outro texto. Os programadores fazem isso para manipular e alterar dados de texto conforme necessário.

## Como fazer:

Aqui está um exemplo de como você pode procurar e substituir texto no Ruby:

```ruby
frase = "Eu amo batatas."
frase.gsub!("batatas", "chocolates")
puts frase
```

A saída seria:

```ruby
"Eu amo chocolates."
```

## Aprofundamento:

### Contexto histórico:

A função de pesquisa e substituição tem sido uma parte fundamental da programação desde o início. Em termos de Ruby, o método `gsub` (substituição global) é um método incorporado na classe `String` que permite a substituição de todas as ocorrências de uma sequência especificada.

### Alternativas:

Ruby oferece mais de uma maneira de realizar a busca e substituição de texto. Além de `gsub`, você também pode usar `sub` para substituir apenas a primeira ocorrência de um padrão em uma string.

```ruby
frase = "Eu amo batatas. Batatas são ótimas."
frase.sub!("Batatas", "Morangos")
puts frase
```

A saída seria:

```ruby
"Eu amo batatas. Morangos são ótimas."
```

### Detalhes de implementação:

Gsub e sub funcionam procurando um padrão especificado em uma string. Você pode especificar esse padrão como uma string ou uma expressão regular. Após encontrar o padrão, gsub ou sub substitui esse padrão com a substituição fornecida.

## Veja também:

Para saber mais sobre busca e substituição de texto em Ruby, considere visitar estes links:

- [Ruby Doc: String#gsub](https://ruby-doc.org/core-2.6.1/String.html#method-i-gsub)
- [Ruby Doc: String#sub](https://ruby-doc.org/core-2.6.1/String.html#method-i-sub)
- [Stackoverflow: Ruby gsub vs sub](https://stackoverflow.com/questions/19445003/using-ruby-gsub-vs-sub)