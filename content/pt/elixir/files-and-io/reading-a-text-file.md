---
date: 2024-01-20 17:54:22.931555-07:00
description: "Ler um arquivo de texto em programa\xE7\xE3o significa acessar e recuperar\
  \ informa\xE7\xF5es de um arquivo salvo em disco. Programadores fazem isso para\
  \ manipular\u2026"
lastmod: '2024-03-13T22:44:46.255302-06:00'
model: gpt-4-1106-preview
summary: "Ler um arquivo de texto em programa\xE7\xE3o significa acessar e recuperar\
  \ informa\xE7\xF5es de um arquivo salvo em disco. Programadores fazem isso para\
  \ manipular\u2026"
title: Lendo um arquivo de texto
weight: 22
---

## O Que & Porquê?
Ler um arquivo de texto em programação significa acessar e recuperar informações de um arquivo salvo em disco. Programadores fazem isso para manipular dados, configurar sistemas ou ler inputs externos.

## Como Fazer:

```elixir
# Para ler um arquivo, utilize File.read:
{status, content} = File.read("meu_arquivo.txt")

# Cheque se foi um sucesso e exiba o conteúdo:
if status == :ok do
  IO.puts("Conteúdo do arquivo:")
  IO.puts(content)
else
  IO.puts("Erro ao ler arquivo: #{status}")
end

# Ou de maneira mais concisa com File.read! que gera um erro se não conseguir ler:
content = File.read!("meu_arquivo.txt")
IO.puts("Conteúdo do arquivo: \n #{content}")
```

Saída de exemplo:
```
Conteúdo do arquivo:
Olá, mundo do Elixir!
```

## Aprofundando:

Historicamente, ler arquivos em linguagens de programação sempre foi uma tarefa essencial porque arquivos são uma forma comum de armazenamento persistente de dados. No Elixir, essa funcionalidade é proporcionada pelo módulo `File`.

Alternativas a `File.read` incluem o `File.stream!`, que permite processar um arquivo linha por linha, reduzindo o uso de memória para arquivos grandes, e `IO.binread/2` para lidar com dados binários.

Detalhes de implementação envolvem coisas como pattern matching com o tuple retornado por `File.read`, tratamento de erros com `File.read!` e uso de outros módulos, como `Stream`, para processamento mais eficiente de dados.

## Veja Também:

- [Elixir Documentation for the File Module](https://hexdocs.pm/elixir/File.html)
- [Elixir School - Leitura de arquivos](https://elixirschool.com/pt/lessons/basics/collections/)
- [Fórum do Elixir: Discussões e dúvidas da comunidade](https://elixirforum.com/)
