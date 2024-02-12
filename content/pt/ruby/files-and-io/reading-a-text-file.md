---
title:                "Lendo um arquivo de texto"
aliases: - /pt/ruby/reading-a-text-file.md
date:                  2024-01-20T17:55:22.117344-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Ler um arquivo de texto em Ruby significa pegar dados de um arquivo `.txt` e usá-los no seu programa. Isso é crucial quando você quer processar informações armazenadas em arquivos, como configurações, dados de entrada para análise e automatizar tarefas que dependam desses dados.

## Como Fazer:

```Ruby
# Lendo um arquivo inteiro
conteudo = File.read('exemplo.txt')
puts conteudo

# Lendo linha por linha
File.foreach('exemplo.txt') do |linha|
  puts linha
end

# Maneira segura usando bloco File.open
File.open('exemplo.txt', 'r') do |arquivo|
  while linha = arquivo.gets
    puts linha
  end
end
```

Saída de exemplo para um arquivo `exemplo.txt` com o conteúdo "Olá, Mundo!":

```
Olá, Mundo!
Olá, Mundo!
Olá, Mundo!
```

## Aprofundando

Historicamente, a leitura de arquivos é tão antiga quanto a própria programação. Nos primórdios da computação, os dados eram lidos de fitas e cartões perfurados. Hoje, ler arquivos é uma operação padrão em qualquer linguagem de programação.

No Ruby, há várias formas de ler um arquivo, como vimos acima. `File.read` carrega o arquivo inteiro na memória, o que é fácil, mas pode ser ineficiente para arquivos muito grandes. `File.foreach` e `File.open` com um bloco são mais eficientes em memória porque processam o arquivo linha por linha.

Outra alternativa é a biblioteca `IO`, da qual `File` é uma subclasse. `IO` fornece métodos de nível mais baixo para controle de arquivos e fluxos de entrada/saída.

Quando ler um arquivo, também é importante considerar a codificação (`encoding`). Ruby suporta múltiplas codificações e permite especificá-las durante a leitura do arquivo.

## Veja Também

- [Ruby-Doc.org para File](https://ruby-doc.org/core-2.7.0/File.html)
- [Ruby-Doc.org para IO](https://ruby-doc.org/core-2.7.0/IO.html)
- [Post no Stack Overflow em inglês sobre a leitura de arquivos grandes](https://stackoverflow.com/questions/25189262/why-is-file-foreach-faster-in-ruby-than-file-read-each-line)
