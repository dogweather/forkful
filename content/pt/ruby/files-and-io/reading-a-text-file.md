---
date: 2024-01-20 17:55:22.117344-07:00
description: "Como Fazer: Sa\xEDda de exemplo para um arquivo `exemplo.txt` com o\
  \ conte\xFAdo \"Ol\xE1, Mundo!\"."
lastmod: '2024-04-05T21:53:47.472050-06:00'
model: gpt-4-1106-preview
summary: "Sa\xEDda de exemplo para um arquivo `exemplo.txt` com o conte\xFAdo \"Ol\xE1\
  , Mundo!\"."
title: Lendo um arquivo de texto
weight: 22
---

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
