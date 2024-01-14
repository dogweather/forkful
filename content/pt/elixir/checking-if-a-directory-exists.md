---
title:    "Elixir: Verificando se um diretório existe"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Verificar se um diretório existe é uma tarefa importante em projetos de programação, especialmente em Elixir. Isso pode ser útil para criar uma lógica condicional em seu código e garantir que o diretório necessário esteja presente antes de prosseguir com determinadas ações.

## Como Fazer

Para verificar se um diretório existe em Elixir, você pode usar a função `File.stat/1` juntamente com o operador `&` para criar uma cláusula de comparação booleana.

```elixir
# Verifica se o diretório "exemplo_diretorio" existe
File.stat("exemplo_diretorio") & &1.type == :directory
```

Esse código irá retornar `true` se o diretório existir e `false` se ele não existir. Você também pode usar a função `File.dir?/1` diretamente para verificar se um diretório existe.

```elixir
# Verifica se o diretório "exemplo_diretorio" existe
File.dir?("exemplo_diretorio")
```

## Profundidade na verificação de diretórios

Ao trabalhar com a verificação de diretórios, é importante considerar os possíveis erros que podem ocorrer. Se o diretório não existir, a função `File.dir?/1` retornará um `false` booleano, mas se ocorrer algum erro durante a verificação, uma exceção será lançada. Portanto, é importante usar a função `File.dir?/2`, que permite passar uma opção adicional de `:raise` para especificar se uma exceção deve ser lançada ou não em caso de erro.

```elixir
# Verifica se o diretório "exemplo_diretorio" existe e não lança uma exceção em caso de erro
File.dir?("exemplo_diretorio", [:raise => false])
```

Além disso, para evitar problemas de concorrência, é importante garantir que a verificação do diretório seja realizada de forma síncrona, usando a função `File.dir?/2` com a opção `:force_sync`. Isso garantirá que não haja problemas de leitura ou gravação simultânea no diretório em questão.

## Veja também

- [Documentação oficial do Elixir sobre verificação de diretórios](https://hexdocs.pm/elixir/File.html#dir?/2)
- [Tutorial sobre como verificar se um diretório existe em Elixir](https://www.tutorialspoint.com/elixir/elixir_check_directory.htm)
- [Discussão sobre os possíveis problemas de concorrência ao verificar diretórios em Elixir](https://elixirforum.com/t/how-to-reliably-check-if-a-directory-exists/37811)