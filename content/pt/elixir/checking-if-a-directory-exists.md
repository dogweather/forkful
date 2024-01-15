---
title:                "Verificando se um diretório existe"
html_title:           "Elixir: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que
Você pode estar se perguntando por que alguém iria querer verificar se um diretório existe. Bem, isso é importante em muitos casos, especialmente quando se lida com a criação ou manipulação de arquivos e pastas em um sistema de arquivos.

## Como fazer
Para verificar se um diretório existe em Elixir, podemos usar a função `File.dir?/1` do módulo `File`. Esta função aceita um caminho para um diretório e retorna `true` se o diretório existe ou `false` se ele não existir.

```
```Elixir
File.dir?("/home/usuario/meu_diretorio") # retorna true
File.dir?("/home/usuario/outro_diretorio") # retorna false
``` 
Também é possível verificar se um caminho é válido antes de utilizá-lo para evitar erros. Para isso, usamos a função `Path.join/2` do módulo `Path` e verificamos seu retorno com `File.dir?/1`.
```
```Elixir
Path.join("home", "usuario", "meu_diretorio") # retorna "/home/usuario/meu_diretorio"
Path.join("home", "usuario", "diretorio_inexistente") # retorna false
File.dir?(Path.join("home", "usuario", "diretorio_inexistente")) # retorna false
```

## Mergulho profundo
A função `File.dir?/1` do módulo `File` utiliza a função `:file.read_dir_c/1` do Erlang por baixo dos panos para obter informações sobre o diretório. Caso o diretório não exista, a função retorna um erro do tipo `%File.Error{reason: :enoent}`. Outra forma de verificar a existência de um diretório é utilizando a função `:file.read_file_info/1`, que retorna informações sobre um arquivo ou diretório, incluindo seu tipo (arquivo ou diretório). Porém, esta função também pode falhar caso o diretório não exista, e seu código de erro é diferente do da função `:file.read_dir_c/1`.

## Veja também
- [Documentação do módulo File](https://hexdocs.pm/elixir/File.html)
- [Documentação do módulo Path](https://hexdocs.pm/elixir/Path.html)
- [Documentação da função `:file.read_dir_c/1`](https://erlang.org/doc/man/file.html#read_dir_c-1)
- [Documentação da função `:file.read_file_info/1`](https://erlang.org/doc/man/file.html#read_file_info-1)