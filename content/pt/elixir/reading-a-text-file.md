---
title:                "Elixir: Lendo um arquivo de texto"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Elixir?

Ler arquivos de texto é uma tarefa comum para muitos programadores, mas é importante entender como fazer isso de forma eficiente e eficaz. Neste post, vamos explorar como ler um arquivo de texto em Elixir e mostrar algumas maneiras de trabalhar com os dados obtidos. Compreender esse processo pode ajudar a melhorar suas habilidades em Elixir e facilitar o seu trabalho com a linguagem.

## Como ler um arquivo de texto em Elixir?

Para ler um arquivo de texto em Elixir, podemos usar a função `File.stream!` ou o módulo `IO` da biblioteca padrão da linguagem. Primeiro, vamos criar um arquivo simples chamado "teste.txt" com alguns dados de exemplo:

```
Elixir é uma linguagem de programação funcional e concorrente.
```

Agora, podemos usar o seguinte código para ler o arquivo em nosso programa:

```
defmodule LeituraArquivo do
  def init do
    {:ok, file} = File.open("teste.txt", [:read])
    
    File.stream!(file)
    |> Stream.map(&String.trim/1)
    |> Enum.to_list()
  end
end
```

Neste exemplo, primeiro abrimos o arquivo usando a função `File.open` e especificamos a opção `[:read]` para indicar que queremos ler os dados do arquivo. Em seguida, usamos `File.stream!` para criar um fluxo que nos permite acessar os dados do arquivo, e usamos o módulo `Stream` para manipulá-los. Por fim, usamos `Enum.to_list` para transformar o fluxo em uma lista de dados que podemos trabalhar.

A saída desse código seria a seguinte:

```
["Elixir é uma linguagem de programação funcional e concorrente."]
```

## Mergulho profundo

Além das funções mencionadas acima, o módulo `IO` também oferece outras opções para manipular arquivos de texto em Elixir. Por exemplo, podemos usar `IO.read` para ler o arquivo inteiro como uma string, ou `IO.binread` para lê-lo como uma lista de bytes. Além disso, o módulo `File` também tem várias funções úteis, como `File.read` e `File.write`, que nos permitem ler e escrever arquivos de texto de forma simples e direta.

Outro ponto importante a ser considerado ao ler arquivos de texto em Elixir é o tratamento de erros. É sempre recomendável usar blocos `do`/`rescue` para lidar com possíveis erros ao ler um arquivo. Isso nos permite tomar medidas apropriadas caso ocorra algum problema durante o processo de leitura.

## Veja também

Aqui estão alguns recursos adicionais para ajudá-lo a explorar mais sobre a leitura de arquivos de texto em Elixir:

- [Documentação oficial do módulo File](https://hexdocs.pm/elixir/File.html)
- [Documentação oficial do módulo IO](https://hexdocs.pm/elixir/IO.html)
- [Página oficial do Elixir](https://elixir-lang.org/)

A leitura de arquivos de texto é uma tarefa simples, mas importante, em muitos projetos de programação. Compreender como fazê-lo corretamente em Elixir pode ser útil tanto para iniciantes quanto para programadores experientes. Esperamos que este post tenha sido útil e que você possa aplicar esses conceitos em seus próprios projetos em Elixir. Até a próxima!