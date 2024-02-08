---
title:                "Criando um arquivo temporário"
date:                  2024-01-20T17:39:50.680198-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Criar um arquivo temporário é uma maneira de gerar um arquivo que só precisa existir durante a execução do seu programa. Programadores fazem isso para guardar dados que não precisam ser mantidos a longo prazo ou para manipular informações sem afetar o estado permanente do sistema.

## How to:
Em Elixir, um arquivo temporário pode ser criado usando a biblioteca padrão `File`. Aqui está um exemplo básico:

```elixir
{:ok, file_path} = File.mktemp()
IO.puts "Arquivo temporário criado em: #{file_path}"

File.write!(file_path, "Algum conteúdo temporário aqui.")
IO.puts "Conteúdo escrito: #{File.read!(file_path)}"

File.rm(file_path)
IO.puts "Arquivo temporário deletado."
```

Saída de exemplo:

```
Arquivo temporário criado em: /tmp/elixir123456
Conteúdo escrito: Algum conteúdo temporário aqui.
Arquivo temporário deletado.
```

## Deep Dive:
Historicamente, arquivos temporários têm sido usados em vários sistemas operacionais para realizar tarefas sem o risco de corromper dados permanentes ou para lidar com grandes volumes de dados de forma eficiente. Há outras formas de se trabalhar com arquivos temporários em Elixir, por exemplo, usando a biblioteca externa `Temp`.

A implementação específica de um arquivo temporário depende do sistema operacional, mas, geralmente, esses arquivos são criados em um diretório especial destinado a este propósito, como `/tmp` no Linux e MacOS, ou `%TEMP%` no Windows.

Arquivos temporários podem ser arriscados se não manipulados corretamente, pois dados sensíveis podem ser deixados para trás acidentalmente. Em Elixir, o `File.rm/1` é uma maneira segura de assegurar que o arquivo é devidamente deletado após o uso.

## See Also:
- [Elixir File Module Documentation](https://hexdocs.pm/elixir/File.html)
- [Using the Temp library for Elixir](https://hex.pm/packages/temp)
