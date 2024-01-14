---
title:    "Ruby: Criando um arquivo temporário"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Ruby?

A criação de arquivos temporários é uma tarefa comum em programação Ruby. Isso pode ser útil ao criar testes, manipular dados temporários ou para outras tarefas que exijam a criação de arquivos temporários.

## Como criar um arquivo temporário em Ruby?

Para criar um arquivo temporário em Ruby, você pode seguir os seguintes passos:

1. Utilize o método `Tempfile.new` para criar o arquivo temporário, especificando o seu nome e o diretório em que ele será criado.
2. Escreva os dados desejados no arquivo temporário utilizando o método `puts`.
3. Utilize o método `unlink` para que o arquivo temporário seja apagado depois de ser usado.

Exemplo de código:

````Ruby
require 'tempfile' # importa a classe Tempfile
temp_file = Tempfile.new('tempfile.txt', '/tmp/') # cria um arquivo temporário com o nome "tempfile.txt" e o salva na pasta /tmp/
temp_file.puts('Este é um arquivo temporário') # escreve a string "Este é um arquivo temporário" no arquivo temporário
temp_file.close # fecha o arquivo temporário
temp_file.unlink # apaga o arquivo temporário
````

Exemplo de saída:

````Ruby
Este é um arquivo temporário
````

## Profundidade da criação de arquivos temporários

Ao criar arquivos temporários em Ruby, é importante ter em mente que o método `Tempfile` cria um arquivo temporário com um nome único. Isso é útil para evitar conflitos de nome entre arquivos temporários. Além disso, o método `Tempfile` também garante que o arquivo temporário seja apagado automaticamente quando o programa termina, evitando que o disco fique cheio de arquivos desnecessários.

Existem também outras formas de criar arquivos temporários em Ruby, como utilizando a biblioteca `tmpdir` ou criando arquivos manualmente com o método `File.open`. No entanto, a utilização do método `Tempfile` é a forma mais segura e eficiente de criar arquivos temporários em Ruby.

## Veja também

- [Documentação do método `Tempfile`](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Documentação da biblioteca `tmpdir`](https://ruby-doc.org/stdlib-2.6.3/libdoc/tmpdir/rdoc/Dir.html)
- [Exemplos de criação de arquivos temporários em Ruby](https://www.rubyguides.com/2015/05/working-with-temporary-files-ruby/)