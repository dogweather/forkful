---
title:    "Elixir: Escrevendo um arquivo de texto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Por que escrever um arquivo de texto em Elixir?

Escrever um arquivo de texto em Elixir pode ser útil em diversas situações, como armazenar dados, gerar relatórios ou criar logs. Além disso, é uma ótima habilidade para aprender no desenvolvimento de aplicações em Elixir.

## Como fazer

Para escrever um arquivo de texto em Elixir, podemos utilizar a função `File.write/2`. Esta função recebe dois argumentos: o primeiro é o nome do arquivo e o segundo é o conteúdo que desejamos escrever. Veja um exemplo abaixo:

```Elixir
File.write("arquivo.txt", "Este texto será escrito no arquivo.")
```

Podemos usar também a função `IO.write/2` para escrever em um arquivo. A diferença é que ela exige que o arquivo esteja aberto em modo de escrita antes de ser chamada. Veja um exemplo:

```Elixir
file = File.open("arquivo.txt", [:write])
IO.write(file, "Este texto também será escrito no arquivo.")
```

O resultado será o mesmo em ambos os casos: o arquivo "arquivo.txt" conterá o texto "Este texto será escrito no arquivo. Este texto também será escrito no arquivo.".

## Mergulho profundo

Ao escrever um arquivo de texto em Elixir, é importante lembrar de fechá-lo após sua utilização. Isso pode ser feito utilizando a função `File.close/1`. Por exemplo:

```Elixir
file = File.open("arquivo.txt", [:write])
IO.write(file, "Este texto será escrito no arquivo.")
File.close(file)
```

Outra coisa importante é que o conteúdo será sempre adicionado ao final do arquivo, caso ele já exista. Se o arquivo não existir, ele será criado automaticamente.

Também é possível controlar a posição onde o conteúdo será adicionado com a função `File.position/2`. Esta função recebe como argumento o arquivo e um número, que indica a posição em bytes onde o conteúdo será inserido. Por exemplo:

```Elixir
file = File.open("arquivo.txt", [:write])
File.position(file, 10)
IO.write(file, "Texto inserido na décima posição.")
```

O texto "Texto inserido na décima posição." será escrito após os primeiros 10 bytes do arquivo.

# Veja também

- [Documentação oficial do Elixir sobre a escrita de arquivos](https://hexdocs.pm/elixir/File.html)
- [Tutorial sobre a manipulação de arquivos em Elixir](https://culttt.com/2017/12/20/working-files-elixir-elixir-alchemy/)