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

# Verifique se um diretório existe com Elixir!

## O que e por que?
Verificar se um diretório existe, é simplesmente uma maneira de confirmar se um diretório específico existe no sistema de arquivos. Os programadores fazem isso para evitar erros ao tentar acessar ou manipular diretórios que podem não existir.

## Como fazer:
Com Elixir é uma tarefa muito fácil. Utilizamos a função `File.dir?/1` da biblioteca padrão `File`.

```elixir
if File.dir?("/caminho/para/seu/diretório") do
  IO.puts "O diretório existe!"
else
  IO.puts "O diretório não existe!"
end
```
Se o diretório existir, o código acima enviará a mensagem "O diretório existe!". Caso contrário, enviará "O diretório não existe!".

## Mergulho profundo

1. **Contexto histórico**: Anteriormente, em sistemas Unix, teríamos que invocar um comando de shell, como `test -d` ou `ls`, para verificar a existência de um diretório. Isso era lento e propenso a falhas.

2. **Alternativas**: Existem várias maneiras de realizar a mesma ação, como usar `:filelib.is_dir/1` do Erlang. No entanto, usar `File.dir?/1` é a maneira mais idiomática em Elixir.

3. **Detalhes de implementação**: `File.dir?/1` está fazendo uma chamada para o sistema operacional subjacente para verificar se o diretório existe. É uma função considerada segura e eficiente para essa tarefa.

## Veja também

1. Documentação oficial Elixir para `File.dir?/1`: [Link](https://hexdocs.pm/elixir/File.html#dir%3F/1)
3. Para mais funções relacionadas a arquivos e diretórios no módulo `File` do Elixir: [Link](https://hexdocs.pm/elixir/File.html)