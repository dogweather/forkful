---
title:                "Lendo argumentos da linha de comando"
html_title:           "Ruby: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Se você já se deparou com programas que têm uma janela preta aparecendo, ou se você já usou o Terminal no seu Mac, então você já se deparou com a entrada de argumentos da linha de comando. Ler esses argumentos é fundamental para entender como controlar e personalizar a execução do seu programa. Neste artigo, vamos aprender a como ler esses argumentos usando Ruby.

## Como fazer

Para ler os argumentos da linha de comando em um programa Ruby, usamos o objeto global `ARGV`. Ele é um array que contém todos os argumentos passados ao programa quando ele foi executado na linha de comando. Vamos ver um exemplo de como ler e utilizar esses argumentos:

```
$ ruby exemplo.rb arg1 arg2
```

Este comando executará o programa Ruby `exemplo.rb` passando dois argumentos: `arg1` e `arg2`. Agora vamos ver como podemos ler esses argumentos dentro do programa:

```ruby
argumento1 = ARGV[0]
argumento2 = ARGV[1]

puts "Argumento 1: #{argumento1}"
puts "Argumento 2: #{argumento2}"
```

O output deste programa será:

```
Argumento 1: arg1
Argumento 2: arg2
```

Perceba que os argumentos são lidos na ordem em que foram passados na linha de comando e a contagem começa em 0, não em 1.

## Mergulho profundo

Existem alguns detalhes adicionais que podem ser úteis ao ler argumentos da linha de comando. Primeiramente, é importante notar que os argumentos são sempre lidos como strings, então é necessário convertê-los para os tipos de dados adequados, se preciso. Além disso, é possível utilizar métodos como `ARGV.size` para saber quantos argumentos foram passados e `ARGV.empty?` para verificar se algum argumento foi passado de fato. 

Você também pode utilizar a flag `--` para indicar o fim dos argumentos na linha de comando. Qualquer coisa inserida depois desta flag será interpretada como um argumento adicional e não será lida pelo programa. 

## Veja também

- [Documentação oficial do Ruby sobre a classe ARGV](https://ruby-doc.org/core-2.7.2/ARGF.html)
- [Artigo da Dev.to sobre leitura de argumentos da linha de comando em Ruby](https://dev.to/soybubba/command-line-arguments-parsing-in-ruby-3ffd)
- [Vídeo tutorial sobre leitura de argumentos da linha de comando em Ruby](https://www.youtube.com/watch?v=JYN87N5NLUQ)