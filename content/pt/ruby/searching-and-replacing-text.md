---
title:                "Buscando e substituindo texto"
html_title:           "Ruby: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Pesquisar e substituir texto é uma técnica comum entre os programadores. É essencialmente a ação de encontrar um determinado pedaço de texto em um arquivo ou código e substituí-lo por outro texto. Os programadores frequentemente usam essa técnica para corrigir erros de digitação, atualizar informações em grandes quantidades de dados ou fazer mudanças em seu código de maneira rápida e eficiente.

## Como fazer:
```
# Exemplo 1: Substituindo uma palavra específica por outra em uma string
texto = "Olá, mundo!"
novo_texto = texto.gsub("mundo", "Ruby")
puts novo_texto
=> "Olá, Ruby!"

# Exemplo 2: Substituindo todas as ocorrências de uma string por outra dentro de um arquivo
arquivo = File.read("arquivo.txt")
novo_arquivo = arquivo.gsub("azul", "vermelho")
File.write("arquivo.txt", novo_arquivo)
```

## Aprofundando:
A técnica de pesquisar e substituir tem sido usada há anos em processamento de texto e edição de documentos. No passado, era comum usar um editor de texto como o Vim ou o Emacs para realizar essas ações. No entanto, com a introdução de linguagens de programação, como o Ruby, essa tarefa se tornou muito mais acessível e eficiente.

Existem também alternativas para pesquisar e substituir texto no Ruby, como o método `sub` que substitui apenas a primeira ocorrência do texto ou o método `gsub!` que modifica diretamente a string em vez de criar uma nova.

Em termos de implementação, o Ruby utiliza expressões regulares para realizar a pesquisa e substituição de texto. Essas são sequências de caracteres especiais utilizadas para encontrar ou validar padrões de texto em uma string. Saber como usar expressões regulares é um conhecimento valioso para ser um programador Ruby eficiente.

## Veja também:
- [Documentação Ruby para `String#gsub`](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)
- [Tutorial sobre como usar expressões regulares em Ruby](https://www.rubyguides.com/ruby-tutorial/ruby-regular-expressions/)