---
title:                "Verificando se um diretório existe"
date:                  2024-01-19
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar se um diretório existe é essencialmente checar se um certo caminho no sistema de arquivos é um diretório. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em um diretório que não existe.

## Como Fazer:
Vamos direto ao código. Verificando a existência de um diretório em Elixir:

```elixir
File.dir?("path/to/directory")
```

Isso retorna `true` se o diretório existir e `false` caso contrário. Exemplo de uso:

```elixir
if File.dir?("/path/to/dir") do
  IO.puts("Diretório existe!")
else
  IO.puts("Diretório não existe!")
end
```

Se `/path/to/dir` existir, você verá:

```
Diretório existe!
```

Caso contrário:

```
Diretório não existe!
```

## Mergulho Profundo
Verificar a existência de diretórios é uma prática comum desde os primórdios da computação, pois é crucial para a integridade e o bom funcionamento dos programas. No Elixir, a função `File.dir?/1` faz parte do módulo `File`, que lida com operações do sistema de arquivos.

Há alternativas para isso em Elixir, como o `File.stat/2` que fornece mais detalhes sobre o caminho especificado, incluindo se é um diretório, e as permissões do mesmo. Contudo, `File.dir?/1` é mais direto para essa verificação específica.

Ao implementar a verificação de um diretório, Elixir interage com o sistema de arquivos subjacente do sistema operacional, portanto a eficácia e o desempenho podem variar entre diferentes sistemas e configurações.

## Veja Também
Para mais informações e contextos além da verificação da existência de diretórios, pode explorar:

- Elixir's `File` module documentation: https://hexdocs.pm/elixir/File.html
- Erlang's file operations (as Elixir is built on the Erlang VM): http://erlang.org/doc/man/file.html
- Guide to file operations in Elixir: https://elixirschool.com/en/lessons/basics/file/
