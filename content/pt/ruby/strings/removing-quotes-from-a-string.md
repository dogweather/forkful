---
date: 2024-01-26 03:41:24.163245-07:00
description: "Como Fazer: O Ruby tem alguns truques interessantes na manga para cortar\
  \ fora essas aspas inc\xF4modas. Voc\xEA pode usar os m\xE9todos `gsub` ou `delete`\
  \ para\u2026"
lastmod: '2024-03-13T22:44:47.082026-06:00'
model: gpt-4-0125-preview
summary: "O Ruby tem alguns truques interessantes na manga para cortar fora essas\
  \ aspas inc\xF4modas."
title: Removendo aspas de uma string
weight: 9
---

## Como Fazer:
O Ruby tem alguns truques interessantes na manga para cortar fora essas aspas incômodas. Você pode usar os métodos `gsub` ou `delete` para realizar essa tarefa. Aqui está um código para refletir:

```ruby
# Usando gsub para remover as aspas duplas e simples
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Saída: Say hello to my little friend!

# Se você souber que lidará apenas com um tipo de aspas
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Saída: Stay a while and listen!
```

## Aprofundamento
A história das aspas remonta aos primeiros dias da programação, onde muitas vezes serviam como delimitadores de strings. Hoje em dia, como antigamente, você pode se encontrar precisando remover esses caracteres de aspas quando não são necessários ou quando poderiam interferir no armazenamento e manipulação de dados.

Falamos sobre `gsub` e `delete`, mas existem outros métodos também, como `tr` ou `tr_s`, que te dão um pouco mais de controle ou podem lidar com casos de uso diferentes:

```ruby
# tr também pode remover aspas
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Saída: Do or do not, there is no try.
```

Lembre-se, cada um desses métodos tem seus casos de uso. `gsub` é mais poderoso quando você está lidando com padrões complexos ou múltiplas substituições. `delete` e `tr` funcionam maravilhosamente para remoções simples e diretas de caracteres.

## Veja Também
Para leitura adicional, e para ver esses métodos em ação dentro de bases de código maiores, confira:
- A documentação do Ruby para [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete) e [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas tem um ótimo [conjunto de exercícios de String](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), que inclui trabalhar com aspas.
