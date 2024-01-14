---
title:                "Elixir: Verificando se um diretório existe"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Muitas vezes, quando estamos escrevendo um programa, é necessário verificar se um diretório existe antes de executar determinadas ações, como criar um novo arquivo ou fazer um backup de arquivos existentes. Fazer essa verificação pode ajudar a evitar erros e problemas no nosso código, tornando-o mais robusto e confiável.

## Como Fazer

Em Elixir, podemos verificar se um diretório existe usando a função `File.dir?/1` e passando o caminho do diretório como argumento. Essa função retorna `true` se o diretório existe ou `false` se não existe. Veja um exemplo:

```Elixir
File.dir?("caminho/do/diretório")
# => true
```

Para trabalhar com diretórios de forma mais eficiente, podemos usar o módulo `Path` e suas funções para lidar com caminhos de arquivos e diretórios. Vamos ver um exemplo de como criar um novo diretório caso ele não exista:

```Elixir
defp create_directory(dir) do
  if !File.dir?(dir) do
    Path.mkdir(dir)
  end
end
```

## Mergulho Profundo

A função `File.dir?/1` simplesmente verifica a existência de um diretório, mas não pode distinguir entre um diretório ou um arquivo com o mesmo nome. Para uma verificação mais precisa, podemos usar a função `Path.wildcard/1` que retorna uma lista de arquivos e diretórios que correspondem a um padrão específico. Um exemplo de código para verificar especificamente se um arquivo existe seria:

```Elixir
defp file_exists?(file) do
  !Path.wildcard(file) == []
end 
```

## Veja Também

- Documentação oficial sobre a função `File.dir?/1` em Elixir (https://hexdocs.pm/elixir/File.html#dir?/1)
- Artigo sobre manipulação de arquivos em Elixir (https://www.infoq.com/br/articles/file-handling-in-elixir/)
- Vídeo tutorial sobre como criar diretórios em Elixir (https://www.youtube.com/watch?v=1kwc44-idx7)