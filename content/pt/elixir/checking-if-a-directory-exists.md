---
title:                "Elixir: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você pode precisar verificar se um diretório específico existe antes de executar alguma ação nele, como criar um novo arquivo ou fazer uma cópia. Verificar a existência de um diretório também pode garantir a segurança e a integridade do seu código, evitando possíveis erros.

## Como fazer

Para verificar se um diretório existe, você pode usar a função `File.dir?/1` na linguagem de programação Elixir. Esta função recebe um caminho de arquivo como argumento e retorna verdadeiro se o caminho do arquivo for um diretório existente ou falso se não existir. Veja um exemplo abaixo utilizando a função `File.dir?/1` para verificar se o diretório "documents" existe:

```elixir
if File.dir?("documents") do
  IO.puts "Diretório documents encontrado!"
else
  IO.puts "Diretório documents não encontrado."
end
```

A saída do código acima será "Diretório documents encontrado!" se o diretório existir ou "Diretório documents não encontrado." se não existir.

## Profundando

No Elixir, você também pode usar o módulo `Path` para verificar a existência de diretórios. A função `Path.expand/2` recebe um diretório e retorna o caminho absoluto para ele. Em seguida, você pode usar a função `File.exists?/1` para verificar se o diretório existe. Veja um exemplo abaixo:

```elixir
path = Path.expand("documents")
if File.exists?(path) do
  IO.puts "Diretório documents encontrado!"
else
  IO.puts "Diretório documents não encontrado."
end
```

Além disso, você também pode usar a função `File.writable?/1` para verificar se o diretório é gravável. Esta função retornará verdadeiro se o diretório for gravável ou falso se não for. Veja um exemplo abaixo:

```elixir
path = Path.expand("documents")
if File.writable?(path) do
  IO.puts "Diretório documents é gravável!"
else
  IO.puts "Diretório documents não é gravável."
end
```

Você pode combinar as funções `File.dir?/1` e `File.writable?/1` para verificar se um diretório existe e se é gravável antes de fazer alterações nele, garantindo assim a segurança do seu código.

## Veja também

- [Documentação oficial do Elixir sobre verificação de diretórios](https://hexdocs.pm/elixir/File.html#dir?/1)
- [Tutorial sobre como criar e verificar diretórios em Elixir](https://www.poeticoding.com/creating-and-checking-directories-in-elixir/)
- [Vídeo mostrando exemplos de como verificar a existência de diretórios em Elixir](https://www.youtube.com/watch?v=ZzA1yC_jO34)