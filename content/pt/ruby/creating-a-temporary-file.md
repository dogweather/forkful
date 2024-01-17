---
title:                "Criando um arquivo temporário"
html_title:           "Ruby: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Criar um arquivo temporário é uma maneira de armazenar informações temporárias durante a execução de um programa. Ele pode ser usado para armazenar dados intermediários de um processo ou como um local para salvar informações temporárias que serão excluídas após o uso. Programadores geralmente criam arquivos temporários para facilitar o gerenciamento de dados durante a execução do código.

## Como fazer:

```Ruby
require 'tempfile'

# Criando um arquivo temporário vazio com uma extensão específica
tempfile = Tempfile.new(['tempo', '.txt'])

# Escrevendo no arquivo temporário
tempfile.write("Este é um arquivo temporário.")

# Lendo o conteúdo do arquivo temporário
tempfile.rewind
puts tempfile.read

# Fechando e excluindo o arquivo temporário
tempfile.close
tempfile.unlink
```

Saída:
```
Este é um arquivo temporário.
```
## Mergulho Profundo:

### Contexto histórico:
A criação de arquivos temporários é um conceito antigo, que remonta aos primeiros dias da programação de computadores. No passado, dia era comum que os programas precisassem salvar dados temporariamente e os arquivos eram frequentemente usados para essa finalidade. Com a evolução da tecnologia, métodos mais avançados de armazenamento de dados temporários foram desenvolvidos, mas a criação de arquivos temporários ainda é amplamente utilizada pelos programadores.

### Alternativas:
Além de criar arquivos temporários, existem outras opções para armazenar dados temporários, como variáveis ou arrays na memória RAM. No entanto, o uso de arquivos temporários pode ser mais eficiente em casos em que é necessário lidar com grandes quantidades de dados.

### Detalhes de implementação:
Em Ruby, a classe `Tempfile` fornece métodos para criar, ler e gerenciar arquivos temporários. O método `new` pode ser usado para criar um arquivo temporário vazio com uma extensão específica. Para escrever e ler dados, os métodos `write` e `read` são utilizados respectivamente. Após o uso, o arquivo temporário deve ser fechado e excluído com os métodos `close` e `unlink`.

## Veja também:

- [Tempfile Class Reference](https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Managing Temporary Files in Ruby](https://www.rubyguides.com/2020/04/ruby-tempfile/)