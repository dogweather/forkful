---
title:                "Removendo aspas de uma string"
aliases:
- /pt/elixir/removing-quotes-from-a-string/
date:                  2024-01-26T03:38:34.822289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Remover aspas de uma string significa descartar aquelas embalagens extras para obter o texto limpo por dentro. Os programadores fazem isso para higienizar a entrada, evitar erros e preparar dados para o processamento onde as aspas são empecilhos, não recursos.

## Como Fazer:
Elixir não possui uma função embutida para 'remover aspas', mas é muito fácil criar a sua própria função utilizando correspondência de padrões ou funções `String`. Veja estes trechos de código:

```elixir
# Usando correspondência de padrões
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Exemplo de Uso
unquote_string("\"Olá, Mundo!\"") # => "Olá, Mundo!"
unquote_string("'Olá, Mundo!'")   # => "Olá, Mundo!"

# Usando String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Exemplo de Uso
unquote_string("\"Olá, Mundo!\"") # => "Olá, Mundo!"
unquote_string("'Olá, Mundo!'")   # => "Olá, Mundo!"
```

A saída para ambos os métodos será:
```
"Olá, Mundo!"
```

## Aprofundamento
Antigamente, as aspas em strings eram um campo minado—manuseá-las de forma inadequada e, puf, erros de sintaxe ou falhas de segurança. No Elixir, a correspondência de padrões trata suas strings como blocos de Lego, permitindo que você as desmonte e remonte com precisão. Seu robusto módulo `String` também é muito útil, eliminando aspas flexivelmente com funções `trim`. As alternativas? Expressões regulares podem descartar as aspas, e bibliotecas externas podem oferecer mais recursos se você precisar de algo além da remoção básica.

## Veja Também
Aprofunde-se com estes recursos:
- [Módulo String do Elixir](https://hexdocs.pm/elixir/String.html)
- [Saiba mais sobre correspondência de padrões no Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Expressões regulares no Elixir (módulo Regex)](https://hexdocs.pm/elixir/Regex.html)
