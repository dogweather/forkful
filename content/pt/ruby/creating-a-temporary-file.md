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

## Por que

Criar arquivos temporários é uma prática comum em muitas linguagens de programação, incluindo Ruby. Isso permite que os programadores criem arquivos temporários durante a execução do programa, que serão automaticamente excluídos quando o programa terminar. Isso pode ser útil para armazenar dados temporários, realizar operações de forma mais eficiente ou garantir que o sistema não seja sobrecarregado com arquivos desnecessários.

## Como fazer

Criar um arquivo temporário em Ruby é bastante simples. Primeiramente, precisamos importar a biblioteca `tempfile`:

```ruby
require 'tempfile'
```

Em seguida, podemos criar o arquivo temporário com o método `Tempfile.new`:

```ruby
temp_file = Tempfile.new('meu_arquivo_temporario')
```

O primeiro parâmetro do método é o prefixo do nome do arquivo, que neste caso será "meu_arquivo_temporario". O arquivo temporário será criado no diretório temporário padrão do sistema operacional.

Para escrever no arquivo, podemos usar o método `write`:

```ruby
temp_file.write('Este é um texto de exemplo')
```

Podemos ler o conteúdo do arquivo usando o método `read`:

```ruby
puts temp_file.read # Saída: Este é um texto de exemplo
```

Após o uso, é importante fechar e deletar o arquivo temporário usando os métodos `close` e `unlink`:

```ruby
temp_file.close
temp_file.unlink
```

Isso garantirá que o arquivo seja excluído corretamente do sistema.

## Profundidade

Uma das vantagens de criar arquivos temporários é que eles são automáticamente excluídos quando o programa termina, o que pode economizar espaço em disco e evitar conflitos com outros programas que usam o mesmo arquivo.

Também é possível especificar o diretório onde o arquivo temporário será criado, ao invés do diretório padrão, usando o parâmetro `dir` do método `Tempfile.new`. Além disso, é possível criar o arquivo em modo de leitura e escrita usando o parâmetro `mode`.

É importante lembrar que os arquivos temporários não são indicados para armazenar dados sensíveis ou importantes, uma vez que eles podem ser facilmente acessados ou excluídos.

## Veja também

- [Documentação da classe Tempfile](https://ruby-doc.org/stdlib-2.7.0/libdoc/tempfile/rdoc/Tempfile.html)
- [Exemplo de uso de arquivo temporário em Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Vantagens e desvantagens de usar arquivos temporários em programas Ruby](https://railsautoscale.com/blog/working-with-temporary-files-in-ruby/)