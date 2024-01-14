---
title:    "Elixir: Lendo argumentos da linha de comando"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que

Escrever programas em Elixir é uma ótima maneira de aproveitar a escalabilidade e a resiliência que a linguagem oferece. Mas para tirar o máximo proveito do Elixir, é importante aprender a ler os argumentos da linha de comando. Aqui estão algumas razões pelas quais você deve se engajar na leitura de argumentos da linha de comando.

## Como fazer

A leitura de argumentos da linha de comando em Elixir é simples e pode ser feita facilmente usando duas funções: `System.argv/0` e `System.argv/1`. A primeira função retorna uma lista de argumentos passados no terminal, enquanto a segunda retorna um único argumento com base em seu índice. A seguir, temos um exemplo de como utilizar essas funções:

```Elixir
# Programa Elixir que recebe dois argumentos na linha de comando e os imprime na tela
defmodule ArgReader do
  def read_args do
    args = System.argv()
    IO.inspect "Primeiro argumento: " <> args[1]
    IO.inspect "Segundo argumento: " <> args[2]
  end
end

# Chama a função read_args
ArgReader.read_args()
```

Na linha de comando, podemos executar este programa passando dois argumentos:
```bash
elixir arg_reader.ex arg1 arg2
```

A saída seria:
```bash
"Primeiro argumento: arg1"
"Segundo argumento: arg2"
```

## Profundidade da mergulho

Além de simplesmente ler argumentos diretamente do terminal, você também pode usar a biblioteca `OptionParser` do Elixir para fornecer argumentos com flags e opções. Isso permite uma maneira mais flexível de passar argumentos e pode ajudar a tornar seus programas mais interativos. Aqui está um exemplo:

```Elixir
# Programa Elixir que utiliza a biblioteca OptionParser para ler argumentos com flags e opções
defmodule ArgParser do
  def read_args do
    # Definindo opções e parâmetros aceitáveis
    options = [%{short: "-h", long: "--help", description: "Ajuda"},
              %{short: "-f", long: "--file", description: "Arquivo para processar"},
              %{short: "-o", long: "--output", type: :binary, description: "Arquivo de saída"}]
    # Utilizando a função OptionParser.parse para analisar os argumentos
    case OptionParser.parse(System.argv(), options) do
      {:ok, parsed_args, _} ->
        # Verificando as opções e realizando a lógica apropriada
        if parsed_args[:help] do
          IO.puts "Opções disponíveis:"
          IO.inspect options
        else
          IO.puts "Processando arquivo #{parsed_args[:file]} e salvando saída em #{parsed_args[:output]}"
        end
      _ ->
        IO.puts "Argumentos inválidos, use --help ou -h para obter ajuda."
    end
  end
end

# Chama a função read_args
ArgParser.read_args()
```

Na linha de comando, podemos executar este programa da seguinte forma:
```bash
elixir arg_parser.ex --file myfile.txt --output results.txt
```

A saída seria:
```bash
Processando arquivo myfile.txt e salvando saída em results.txt
```

## Veja também

- [Documentação oficial do Elixir sobre leitura de argumentos da linha de comando](https://hexdocs.pm/elixir/System.html#argv/0)
- [Biblioteca OptionParser do Elixir](https://hexdocs.pm/elixir/OptionParser.html)