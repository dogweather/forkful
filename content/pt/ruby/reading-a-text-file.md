---
title:                "Ruby: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que Ler um Arquivo de Texto em Ruby?

Ler arquivos de texto é uma habilidade essencial para qualquer programador. Com a linguagem de programação Ruby, ler arquivos de texto pode ser uma tarefa simples e eficiente. Neste post, vamos explorar o porquê de ler arquivos de texto e como fazer isso usando Ruby. 

## Como Ler um Arquivo de Texto em Ruby

Ler um arquivo de texto em Ruby envolve alguns passos simples. Primeiro, precisamos abrir o arquivo e depois ler seu conteúdo. Vamos ver um exemplo de como podemos fazer isso:

```
arquivo = File.open("texto.txt")
puts arquivo.read
```

No exemplo acima, estamos abrindo um arquivo chamado "texto.txt" e usando o método `read` para ler seu conteúdo e imprimi-lo na tela. Podemos também armazenar o conteúdo do arquivo em uma variável para usá-lo posteriormente:

```
arquivo = File.open("texto.txt")
conteudo = arquivo.read
puts conteudo
```

A saída acima será o conteúdo do arquivo "texto.txt".

## Mergulho Profundo

Existem vários métodos que podemos usar para ler arquivos de texto em Ruby. Além do `read`, podemos usar o método `readline`, que lê uma linha por vez do arquivo, ou o método `readlines`, que lê todas as linhas do arquivo e as armazena em um array. Além disso, é importante ter cuidado ao lidar com arquivos grandes, pois podemos acabar ocupando muita memória do sistema. Nesses casos, é recomendado o uso de blocos para garantir o fechamento automático do arquivo após o uso. 

## Veja Também

- [Documentação oficial da classe File em Ruby](https://ruby-doc.org/core-3.0.0/File.html)
- [Tutorial de Ruby: Trabalhando com Arquivos](http://rubylearning.com/satishtalim/working_with_files.html)
- [Exemplos práticos de leitura de arquivos em Ruby](https://www.freecodecamp.org/news/learn-to-read-files-in-ruby/)