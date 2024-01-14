---
title:    "Elixir: Verificando se um diretório existe"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Existem várias razões pelas quais um desenvolvedor pode precisar verificar se um diretório existe. Uma das principais razões é garantir que o código não falhe ao tentar acessar um diretório inexistente. Verificar a existência de um diretório antes de tentar acessá-lo pode tornar seu código mais robusto e evitar possíveis erros.

## Como verificar se um diretório existe em Elixir

Verificar se um diretório existe em Elixir é bastante simples. Basta usar a função `File.dir?/1`, passando o caminho do diretório como argumento. Por exemplo:

```Elixir
File.dir?("caminho/do/diretório")
# => true or false
```

Se o diretório existir, a função retornará `true`. Caso contrário, retornará `false`. Você também pode usar `File.dir?/1` para verificar se um arquivo existe, passando o caminho do arquivo como argumento.

## Aprofundando mais

Além da função `File.dir?/1`, existem outras maneiras de verificar a existência de um diretório em Elixir. Você pode usar a função `File.ls!/1` para listar todos os arquivos e diretórios em um determinado caminho e, em seguida, verificar se o diretório desejado está na lista. Outra opção é usar a função `Dir.exists?/1`, que retorna `true` se o diretório existir e `false` se não existir.

Uma consideração importante ao verificar a existência de um diretório é garantir que o caminho fornecido seja absoluto e não relativo. Isso garante que o diretório seja procurado no local correto.

## Veja também

- [Documentação oficial do Elixir sobre File](https://hexdocs.pm/elixir/File.html)
- [Mais exemplos de como trabalhar com arquivos em Elixir](https://elixirschool.com/lessons/basics/file-operations/)
- [Tutorial sobre como criar e gerenciar diretórios em Elixir](https://www.codementor.io/@calebhearth/how-to-create-and-manage-directories-in-elixir-q20ee507f)