---
title:                "Ruby: Criando um arquivo temporário"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Ruby

Criar arquivos temporários é uma tarefa comum em muitos projetos de programação Ruby. Eles são úteis para armazenar informações temporárias que precisam ser acessadas ou manipuladas durante a execução do programa. Alguns exemplos incluem o armazenamento de dados temporários durante a criação de relatórios ou a realização de testes unitários.

## Como criar um arquivo temporário em Ruby

Criar um arquivo temporário em Ruby é uma tarefa relativamente simples. Podemos usar o método `Tempfile.create` seguido pelo caminho de destino e, opcionalmente, o prefixo e sufixo do nome do arquivo. Veja um exemplo abaixo:

```ruby
require 'tempfile'

# Criando um arquivo temporário sem prefixo e sufixo
Tempfile.create("relatorio") do |file|
  # Realizando operações com o arquivo temporário
  # ...

  # Ao final, o arquivo temporário é excluído automaticamente
end
```

O método `Tempfile.create` retorna um objeto `Tempfile` que podemos usar para escrever ou ler dados do arquivo temporário. Ao final do bloco, o arquivo é automaticamente excluído. Também é possível especificar um caminho específico para o arquivo temporário, se necessário.

## Aprofundando na criação de um arquivo temporário

Ao criarmos um arquivo temporário em Ruby, ele é automaticamente excluído após a execução do programa. Isso evita que nosso sistema seja sobrecarregado com arquivos inúteis. No entanto, é importante lembrar que, se o arquivo temporário for usado em loop ou em uma thread separada, ele só será excluído quando o programa inteiro terminar sua execução.

Podemos definir algumas opções adicionais ao criar um arquivo temporário, como o modo de abertura do arquivo (leitura, gravação, etc.), as permissões do arquivo e até mesmo o diretório onde o arquivo será criado. Usando essas opções, podemos personalizar a criação de um arquivo temporário de acordo com as necessidades do nosso projeto.

## Veja também

- [Documentação do método Tempfile.create na classe Tempfile](https://ruby-doc.org/stdlib-2.7.0/libdoc/tempfile/rdoc/Tempfile.html#method-c-create)
- [Como criar e gerenciar arquivos temporários em Ruby](https://www.rubyguides.com/2017/01/working-with-temporary-files-ruby/)