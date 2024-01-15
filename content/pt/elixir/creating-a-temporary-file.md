---
title:                "Criando um arquivo temporário"
html_title:           "Elixir: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de criar um arquivo temporário durante a execução de seu programa em Elixir? Esses arquivos temporários oferecem uma maneira conveniente de armazenar dados temporários durante a execução do seu código.

## Como fazer

Para criar um arquivo temporário em Elixir, podemos usar a biblioteca padrão `:os.tmp_dir/0` para obter o caminho do diretório temporário do sistema operacional. Em seguida, podemos usar a função `:tempfile.open/2` para criar um arquivo temporário neste diretório e retornar um PID para manipulá-lo.

```
Elixir
# Obtemos o caminho do diretório temporário do sistema operacional
path = :os.tmp_dir

# Criamos um arquivo temporário neste diretório e retornamos um PID
{:ok, pid} = :tempfile.open([path])
```

Agora podemos usar o PID para realizar operações de escrita e leitura no arquivo temporário.

```
Elixir
# Escrevemos no arquivo temporário
:ok = :file.write(pid, "Este é um arquivo temporário")

# Lemos o conteúdo do arquivo
content = :file.read(pid)

# Imprimimos o conteúdo para a saída
IO.puts(content)
```

No exemplo acima, usamos a função `:file.write/2` para escrever no arquivo e `:file.read/1` para ler seu conteúdo. Finalmente, podemos usar a função `:file.close/1` para fechar o arquivo temporário quando não precisarmos mais dele.

```
Elixir
# Fechamos o arquivo temporário
:ok = :file.close(pid)
```

## Deep Dive

Ao criar um arquivo temporário em Elixir, é importante lembrar que ele será excluído automaticamente após o fechamento do PID. No entanto, se quisermos manter o arquivo temporário após o fechamento do PID, podemos usar a opção `:keep` na função `:tempfile.open/2`.

```
Elixir
# Criamos um arquivo temporário que será mantido após o fechamento do PID
{:ok, pid} = :tempfile.open([path], [:keep])
```

Também podemos especificar o nome do arquivo temporário que será criado usando a opção `:prefix`. Isso é útil quando precisamos de um arquivo com um nome específico para lidar com ele em outras partes do código.

```
Elixir
# Criamos um arquivo temporário com um prefixo específico
{:ok, pid} = :tempfile.open([path], [:prefix, "meu_arquivo"])
```

## Veja também

- Documentação oficial do Elixir: https://hexdocs.pm/elixir/
- Tutorial de arquivos temporários em Elixir: https://elixirschool.com/pt/lessons/advanced/files/
- Artigo sobre arquivos e diretórios em Elixir: https://thinkingelixir.com/directories-files-and-absolute-paths-in-elixir/