---
title:                "Lendo um arquivo de texto"
html_title:           "Ruby: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?

Ler um arquivo de texto é uma tarefa comum para programadores, pois permite acessar e manipular informações armazenadas em um arquivo. Isso é útil para ler dados de entrada para o programa, como configurações ou dados de usuários, ou para escrever dados de saída, como resultados ou logs.

## Como fazer:

Para ler um arquivo de texto em Ruby, podemos usar o método `File.open`, passando o caminho do arquivo e o modo de acesso como argumentos. Por exemplo, para ler um arquivo `texto.txt` no modo leitura, usamos:

```Ruby
File.open('texto.txt', 'r') do |arquivo|
  # faça algo com o conteúdo do arquivo
end
```

Dentro do bloco, podemos usar o método `#read`, que retorna todo o conteúdo do arquivo como uma string, ou `#readlines`, que retorna um array de linhas do arquivo.

```Ruby
File.open('texto.txt', 'r') do |arquivo|
  conteudo = arquivo.read
  puts conteudo # imprime o conteúdo do arquivo
end

File.open('texto.txt', 'r') do |arquivo|
  linhas = arquivo.readlines
  puts linhas[0] # imprime a primeira linha
end
```

## Mergulho profundo

Ler arquivos de texto é uma tarefa simples e comum em muitas linguagens de programação. Na verdade, quase todas as linguagens possuem algum método ou função dedicado a essa tarefa. Em Ruby, além do `File.open`, também podemos usar o método `IO.read`, que lê todo o conteúdo do arquivo em uma string, ou `IO.foreach`, que itera sobre cada linha do arquivo.

Além disso, também podemos especificar o modo de acesso ao abrir o arquivo, como `w` para escrita e `a` para append. No entanto, é importante lembrar de fechar o arquivo após usá-lo para liberar os recursos do sistema operacional.

## Veja também

- Documentação oficial do método `File.open`: https://ruby-doc.org/core-3.0.0/File.html#method-c-open
- Artigo sobre manipulação de arquivos em Ruby: https://www.rubyguides.com/2015/05/working-with-files-ruby/
- Discussão sobre alternativas ao `File.open`: https://stackoverflow.com/questions/14328124/alternatives-to-file-open-in-ruby