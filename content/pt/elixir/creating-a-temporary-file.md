---
title:    "Elixir: Criando um arquivo temporário"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Elixir?

Criar arquivos temporários é uma prática comum em programação e pode ser útil por razões como armazenar dados temporariamente ou como parte de um processo de gerenciamento de arquivos. Em Elixir, criar um arquivo temporário também pode ser útil para tarefas como testes ou integração com bibliotecas externas.

## Como fazer isso em Elixir

A seguir, veremos como criar um arquivo temporário em Elixir usando o módulo `File`.

```Elixir
# Importar o módulo File
import File

# Definir o nome do arquivo temporário
file_name = "temp.txt"
# Definir o conteúdo a ser escrito no arquivo
content = "Este é um arquivo temporário criado em Elixir."

# Usar a função open/2 do módulo File para criar o arquivo
{:ok, file} = File.open(file_name, [:write])

# Escrever o conteúdo no arquivo
IO.write(file, content)
# Fechar o arquivo
File.close(file)
```

Ao executar este código, um novo arquivo temporário será criado no diretório atual com o nome "temp.txt" e o conteúdo definido será escrito nele.

## Mergulhando mais fundo

Em Elixir, também é possível utilizar o módulo `Tempfile` para criar arquivos temporários. Este módulo fornece funções úteis para lidar com arquivos temporários, como `open/2`, `write/2` e `close/1`.

Além disso, é importante lembrar de excluir os arquivos temporários após o uso, para evitar a acumulação desnecessária de arquivos em nosso sistema. Para isso, podemos usar a função `File.rm/1`, que exclui um arquivo específico.

## Veja também

- [Documentação do módulo File](https://hexdocs.pm/elixir/File.html)
- [Documentação do módulo Tempfile](https://hexdocs.pm/tempfile/Tempfile.html)