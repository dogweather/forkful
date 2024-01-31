---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
simple_title:         "Escrevendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever um arquivo de texto é registrar dados em um arquivo que qualquer editor de texto pode abrir. Programadores fazem isso para salvar configurações, dados de output ou para criar scripts utilizáveis.

## Como Fazer:

```elixir
# Cria ou sobre-escreve um arquivo chamado "exemplo.txt" com o conteúdo "Olá, Elixir!"

File.write!("exemplo.txt", "Olá, Elixir!\n")

# Para acrescentar conteúdo em vez de sobre-escrever, use `File.write/3` com o modo `:append`

File.write("exemplo.txt", "Mais texto.", [:append])
```

### Exemplo de Output:

Depois de rodar o código acima, "exemplo.txt" terá o seguinte conteúdo:

```
Olá, Elixir!
Mais texto.
```

## Mergulho Profundo:

Historicamente, escrever em arquivos é um conceito que data da época em que os programas eram gravados em fita. No Elixir, a abstração para IO permite escrever em arquivos de maneira direta e funcional. Alternativas incluem usar streams para lidar com grandes volumes de dados ou bancos de dados para persistência a longo prazo. Quando se trata da implementação, Elixir trata arquivos como processos, o que significa que escrever para arquivos é efetivamente uma comunicação de mensagens.

## Veja Também:

- [Documentação oficial do módulo File do Elixir](https://hexdocs.pm/elixir/File.html)
- [Guia Introdutório de Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Discussões sobre manipulação de arquivo com Elixir no Elixir Forum](https://elixirforum.com/search?q=file%20handling)
