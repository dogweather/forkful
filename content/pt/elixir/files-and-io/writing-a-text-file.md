---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:38.478600-07:00
description: "Escrever em um arquivo de texto em Elixir \xE9 uma habilidade essencial\
  \ para desenvolvedores, permitindo a persist\xEAncia de dados, registro (logging)\
  \ ou\u2026"
lastmod: '2024-03-13T22:44:46.256265-06:00'
model: gpt-4-0125-preview
summary: "Escrever em um arquivo de texto em Elixir \xE9 uma habilidade essencial\
  \ para desenvolvedores, permitindo a persist\xEAncia de dados, registro (logging)\
  \ ou\u2026"
title: Escrevendo um arquivo de texto
weight: 24
---

## O Que & Por Que?

Escrever em um arquivo de texto em Elixir é uma habilidade essencial para desenvolvedores, permitindo a persistência de dados, registro (logging) ou exportação de conteúdo legível por humanos. Os programadores realizam isso para salvar o estado da aplicação, informações de depuração, configurações ou qualquer troca de dados entre sistemas que prefiram um formato ubíquo como texto.

## Como fazer:

Elixir torna o manuseio de arquivos direto com módulos integrados. A principal maneira de escrever em um arquivo é usando as funções `File.write/2` ou `File.write!/2`, onde a primeira retorna uma tupla `:ok` ou `:error` e a última gera um erro em caso de falha.

Aqui está um exemplo simples:

```elixir
# Escrevendo em um arquivo, mensagem simples
File.write("hello.txt", "Olá, Mundo!")

# Quando você executa o código, ele cria 'hello.txt' com "Olá, Mundo!" como conteúdo
```

Para acrescentar a arquivos, você usaria `File.open/3` com as opções `[:write, :append]`, depois `IO.binwrite/2` para acrescentar o conteúdo:

```elixir
# Acrescentando a um arquivo
{:ok, arquivo} = File.open("hello.txt", [:write, :append])
IO.binwrite(arquivo, "\nVamos adicionar outra linha.")
File.close(arquivo)

# Agora 'hello.txt' inclui uma segunda linha "Vamos adicionar outra linha."
```

Se você está trabalhando com grandes volumes de dados ou precisa de mais controle sobre o processo de escrita, você pode usar o módulo `Stream` para escrever dados de forma preguiçosa no arquivo:

```elixir
# Escrevendo um grande conjunto de dados de maneira preguiçosa
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Número: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn arquivo ->
  Enum.each(stream_data, fn linha ->
    IO.write(arquivo, linha)
  end)
end)

# Isso cria 'numbers.txt', escrevendo os números de 0 a 9, cada um em uma nova linha.
```

Para projetos que requerem manuseio de arquivos mais sofisticado, você pode investigar bibliotecas de terceiros como `CSV`, que oferece funcionalidades sob medida para manipulação de arquivos CSV, mas lembre-se, para muitos propósitos, as capacidades integradas do Elixir são mais do que suficientes.
