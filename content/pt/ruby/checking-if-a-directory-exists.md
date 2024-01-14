---
title:                "Ruby: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Verificar se um diretório existe pode ser uma tarefa importante em alguns projetos de programação Ruby. Isso pode ser necessário para garantir que um determinado diretório exista antes de tentar ler ou gravar arquivos nele. Também pode ser usado para verificar se um diretório foi criado corretamente durante a execução de um programa.

## Como fazer

Para verificar se um diretório existe em Ruby, podemos usar o método `Dir.exist?`. Este método retorna um valor booleano (verdadeiro ou falso) com base na existência do diretório especificado.

Vamos ver um exemplo de como usar o método `Dir.exist?` em um código Ruby:

```ruby
if Dir.exist?("/caminho/para/diretorio")
  puts "O diretório existe!"
else
  puts "O diretório não existe."
end
```

A saída deste código será `O diretório existe!`, isso significa que o diretório especificado existe. Se o caminho do diretório estiver incorreto, a saída será `O diretório não existe.`.

Outra forma de verificar a existência de um diretório é usar o operador de negação `!` em conjunto com o método `Dir.exist?`:

```ruby
unless !Dir.exist?("/caminho/para/diretorio")
  puts "O diretório existe!"
else
  puts "O diretório não existe."
end
```

Novamente, a saída será `O diretório existe!` se o diretório existir e `O diretório não existe.` se não existir.

## Profundidade

Você pode estar se perguntando como o método `Dir.exist?` funciona por trás dos panos. Na verdade, ele é uma simples verificação do sistema de arquivos do seu computador. Se o caminho especificado for encontrado, ele retornará verdadeiro, caso contrário, retornará falso.

Outra opção é usar o método `File.exist?` que funciona da mesma forma, mas pode ser usado para verificar a existência de um arquivo ao invés de um diretório.

## Veja também

- [Documentação oficial do Ruby sobre o método Dir.exist?](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Exemplo de verificação de diretório em Ruby](https://www.geeksforgeeks.org/ruby-check-if-the-dictionary-is-empty-or-not/)
- [Guia completo sobre diretórios em Ruby](https://www.rubyguides.com/2017/08/ruby-directory/)