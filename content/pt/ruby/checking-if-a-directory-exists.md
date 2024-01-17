---
title:                "Verificando se um diretório existe"
html_title:           "Ruby: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Verificar se um diretório existe é uma forma de garantir que o caminho especificado para um diretório é válido e pode ser acessado pelo programa. Isso é importante porque, ao tentar acessar um diretório que não existe, o programa pode encontrar erros e falhar.

## Como fazer:
```ruby
Dir.exist?("/path/to/directory")
```
Saída:
```ruby
true
```

```ruby
Dir.exist?("/path/to/nonexistent/directory")
```
Saída:
```ruby
false
```

## Mergulho Profundo:
Verificar a existência de diretórios é uma prática comum entre os programadores, especialmente ao lidar com arquivos e pastas em sistemas de arquivos. Antes da versão 1.9 do Ruby, a maneira de verificar se um diretório existia era usando o método `File.directory?`. No entanto, a partir da versão 1.9, esse método foi depreciado em favor do método `Dir.exist?`.

Além disso, existem outras formas de verificar a existência de um diretório, como usar comandos de sistema operacional no Ruby. Por exemplo, usando o comando `system` seguido de `mkdir`, é possível criar um diretório se ele não existir. Outra alternativa é usar o método `Dir.mkdir` que também cria um diretório se ele não existir.

## Veja também:
- [Documentação Oficial do Ruby - Classe Dir](https://ruby-doc.org/core-3.0.1/Dir.html)
- [Ruby Guides - Trabalhando com diretórios no Ruby](https://www.rubyguides.com/2015/04/working-with-directories-in-ruby/)