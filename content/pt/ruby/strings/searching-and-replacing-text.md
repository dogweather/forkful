---
aliases:
- /pt/ruby/searching-and-replacing-text/
date: 2024-01-20 17:58:36.268388-07:00
description: "Procurar e substituir texto \xE9 tipo fazer uma ca\xE7a ao tesouro e\
  \ trocar o que voc\xEA encontrou. Programadores fazem isso para corrigir erros,\
  \ atualizar dados\u2026"
lastmod: 2024-02-18 23:08:58.648958
model: gpt-4-1106-preview
summary: "Procurar e substituir texto \xE9 tipo fazer uma ca\xE7a ao tesouro e trocar\
  \ o que voc\xEA encontrou. Programadores fazem isso para corrigir erros, atualizar\
  \ dados\u2026"
title: Pesquisando e substituindo texto
---

{{< edit_this_page >}}

## What & Why?
Procurar e substituir texto é tipo fazer uma caça ao tesouro e trocar o que você encontrou. Programadores fazem isso para corrigir erros, atualizar dados ou simplificar código, sem perder tempo.

## How to:
```Ruby
texto = "Eu amo programar em Ruby!"
novo_texto = texto.gsub("amo", "adoro")
puts novo_texto
# => Eu adoro programar em Ruby!

# Procurando com regex
resultado = texto.scan(/a[\w]o/)
puts resultado
# => ["amo"]

# Substituindo com regex
texto_fix = texto.gsub(/a[\w]o\b/, 'odeio')
puts texto_fix
# => Eu odeio programar em Ruby!
```

## Deep Dive
Procurar e substituir em Ruby não é novidade. Desde os primórdios, Ruby teve métodos como `gsub` e `sub` para substituir texto, e `scan` para procurar. Regex, ou expressões regulares, são ferramentas poderosíssimas nesse processo, permitindo padrões complexos e substituições condicionais.

Alternativas envolvem bibliotecas externas ou linguagens de script, como sed em UNIX, mas Ruby já vem com tudo o que é necessário. Na implementação, `gsub` cria uma nova string, eficiência que pode ser crítica em textos longos. `sub` tem performance similar, mas altera só a primeira ocorrência.

## See Also
- Ruby's `String` documentation: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Tutorial sobre expressões regulares em Ruby: [rubular.com](http://rubular.com/)
