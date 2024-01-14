---
title:                "Ruby: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Por que criamos arquivos temporários no Ruby

Criar arquivos temporários pode ser uma tarefa muito útil quando estamos trabalhando com dados temporários que não são necessários por muito tempo. Alguns cenários podem incluir a geração de arquivos de log, armazenamento de dados temporários durante a execução de um programa ou testes unitários.

##Como fazer

Existem várias maneiras de criar arquivos temporários em Ruby, mas a maneira mais comum é usando a classe Tempfile. Vamos dar uma olhada em um exemplo simples de como criar um arquivo temporário e escrever algumas linhas de texto nele:

```Ruby
require 'tempfile'
tempfile = Tempfile.new('temp_file')
tempfile.write('Esse é um arquivo temporário.')
tempfile.close
```

Aqui, usamos o método `require` para carregar a biblioteca Tempfile, em seguida, usamos o método `new` para criar um objeto Tempfile com o prefixo "temp_file". Em seguida, escrevemos uma string no arquivo temporário usando o método `write` e finalmente fechamos o arquivo usando o método `close`.

Para ler o conteúdo do arquivo temporário, podemos usar o método `read`:

```Ruby
puts File.read(tempfile)
```

O output seria:

```
Esse é um arquivo temporário.
```

##Mergulho profundo

A classe Tempfile oferece várias opções para criar e manipular arquivos temporários. Por exemplo, podemos especificar a pasta onde o arquivo temporário será criado, o modo de acesso e as permissões do arquivo. Além disso, podemos usar os métodos `unlink` ou `close!` para excluir o arquivo temporário quando não precisamos mais dele.

Também é possível criar arquivos temporários com uma extensão específica usando o método `make_tmpname`:

```Ruby
tempfile = Tempfile.make_tmpname(["temp", ".txt"], "/tmp")
```

Podemos encontrar mais informações sobre a classe Tempfile e suas opções na documentação oficial do Ruby.

##Veja também

- [Documentação oficial do Ruby sobre a classe Tempfile](https://ruby-doc.org/stdlib-2.6.5/libdoc/tempfile/rdoc/Tempfile.html)
- [Tutorial sobre como criar arquivos temporários em Ruby](https://blog.appsignal.com/2019/09/30/how-to-create-temporary-files-in-ruby.html)