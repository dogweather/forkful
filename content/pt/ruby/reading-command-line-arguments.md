---
title:                "Ruby: Lendo Argumentos da Linha de Comando"
simple_title:         "Lendo Argumentos da Linha de Comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando por que seria importante aprender a ler argumentos de linha de comando em Ruby. A resposta é simples – essa habilidade pode ser muito útil, especialmente quando se trata de trabalhar com programas do tipo "linha de comando" ou "terminal". Isso permite que você interaja diretamente com seu código e personalize a entrada de dados.

## Como Fazer

Ruby facilita muito a leitura de argumentos de linha de comando. Tudo o que você precisa é utilizar a classe `ARGV`, que é responsável por armazenar os argumentos passados durante a execução do programa.

```Ruby
# Lendo o primeiro argumento
puts ARGV[0]

# Lendo o segundo argumento
puts ARGV[1]

# Lendo todos os argumentos em um loop
ARGV.each do |arg|
  puts arg
end
```

Agora, vamos supor que você queira rodar o seguinte comando: `ruby meu_programa.rb argumento1 argumento2`. O output seria:

```
argumento1
argumento2
argumento1
argumento2
```

## Mergulho Profundo

Além da classe `ARGV`, você também pode utilizar o método `gets` para ler argumentos de linha de comando. Ele permite que você faça a leitura de entrada de dados diretamente do terminal, o que pode ser útil em certas situações.

```Ruby
print "Digite seu nome: "
nome = gets.chomp
puts "Olá, #{nome}!"
```

Este código irá pedir ao usuário que digite seu nome, e em seguida, irá imprimir uma mensagem personalizada com base na entrada. É importante notar que o método `gets` sempre retorna uma string, então é necessário utilizar o método `chomp` para remover a quebra de linha que é acrescentada à entrada.

## Veja Também

* [Ruby - Documentação Oficial](https://www.ruby-lang.org/pt/documentation/)
* [Introdução ao Ruby para Iniciantes](https://medium.com/@qbustillo91/introdu%C3%A7%C3%A3o-ao-ruby-para-iniciantes-68a64b031fb6)
* [Aprenda Ruby em 20 Minutos](https://www.youtube.com/watch?v=Dji9ALCgfpM)