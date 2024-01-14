---
title:                "Elixir: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando com Elixir?

Quando se trabalha com programas em Elixir, é comum a necessidade de receber informações do usuário através de argumentos na linha de comando. Ler esses argumentos é importante para tornar o programa mais interativo e dinâmico, permitindo que o usuário forneça entradas personalizadas de acordo com suas necessidades. Neste artigo, vamos explorar como ler argumentos da linha de comando com Elixir e a importância disso para o desenvolvimento de programas eficientes.

## Como fazer:

Para ler argumentos da linha de comando em Elixir, podemos utilizar a função `System.argv/0`, que retorna uma lista com os argumentos passados pelo usuário. Por exemplo:

```Elixir
# Lendo e imprimindo o primeiro argumento
IO.puts("O primeiro argumento é #{System.argv[0]}")

# Lendo e armazenando todos os argumentos em uma lista
args = System.argv()
IO.inspect(args)
```

A saída do primeiro exemplo seria: `O primeiro argumento é hello.exs`, considerando que o arquivo está sendo executado como `elixir hello.exs ola`. Já a saída do segundo exemplo seria uma lista com os argumentos `["ola"]`.

Podemos também utilizar a função `Kernel.get_arguments/0` para obter os argumentos de forma mais estruturada, retornando uma lista de pares chave-valor. Por exemplo:

```Elixir
# Lendo e imprimindo os argumentos de forma mais estruturada
args = Kernel.get_arguments()
IO.inspect(args)
```

A saída seria: `[{"option", "hello"}, {"value", "ola"}]` para o exemplo acima.

## Mergulho profundo:

Além de receber argumentos na linha de comando, também podemos passar opções para o programa. Essas opções são muito úteis para definir comportamentos diferentes ou personalizar a execução do programa. Para isso, podemos utilizar a biblioteca `OptionParser` do Elixir, que nos permite parsear os argumentos da linha de comando e definir opções com seus respectivos valores.

Por exemplo, vamos definir uma opção `--name` que recebe um valor como argumento e, caso seja passada, irá imprimir uma mensagem personalizada com o nome fornecido:

```Elixir
# Definindo as opções do programa
options = OptionParser.parse(
  [
    [short: "--name", description: "Define o nome para a saudação"]
  ],
  aliases: [n: :name]
)

case options do
  {[:ok, opts], _, _} ->
    # Verificando se a opção --name foi passada
    case opts[:name] do
      name when is_binary(name) ->
        IO.puts("Olá, #{name}!")
      _ ->
        IO.puts("Ola!")
    end
  _ ->
    # Imprimindo mensagem padrão caso não seja passada nenhuma opção
    IO.puts("Ola!")
end
```

Agora, quando executamos o programa utilizando `elixir hello.exs -n Jon`, a saída seria `Olá, Jon!`, enquanto simplesmente executar `elixir hello.exs` resultaria em `Ola!`.

## Veja também:

- [Documentação sobre argumentos da linha de comando no Elixir](https://hexdocs.pm/elixir/System.html#argv/0)
- [Documentação sobre a biblioteca OptionParser no Elixir](https://hexdocs.pm/elixir/OptionParser.html)
- [Artigo sobre leitura de argumentos da linha de comando em Elixir](https://dev.to/diana_adrianne/how-to-read-command-line-arguments-in-elixir-2n9k)