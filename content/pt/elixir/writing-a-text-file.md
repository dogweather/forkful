---
title:    "Elixir: Escrevendo um arquivo de texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Elixir?

Escrever um arquivo de texto é uma tarefa comum em muitas linguagens de programação, e Elixir não é exceção. Ao escrever um arquivo de texto, você pode armazenar informações importantes que seu programa precisa acessar, como dados de configuração ou mensagens de erro. Além disso, os arquivos de texto são uma forma simples e acessível de compartilhar informações com outros programas ou usuários.

## Como fazer isso em Elixir?

Para escrever um arquivo de texto em Elixir, você pode usar a função `File.write/2`. Veja um exemplo de código abaixo:

```elixir
# Cria um arquivo chamado "dados.txt" e escreve a string "Olá mundo!" nele
File.write("dados.txt", "Olá mundo!")
```

Este código criará um arquivo chamado "dados.txt" no diretório atual e escreverá a string "Olá mundo!" nele. Você também pode adicionar opções adicionais para personalizar como e onde o arquivo será criado, como por exemplo o modo de escrita e as permissões do arquivo. Consulte a documentação oficial para mais detalhes.

## Profundidade na escrita de arquivos de texto em Elixir

Existem muitas nuances e detalhes a serem considerados ao escrever arquivos de texto em Elixir. Por exemplo, você precisa garantir que o arquivo exista antes de tentar escrever nele, caso contrário você receberá um erro. Além disso, é importante considerar o desempenho ao escrever grandes quantidades de dados em um arquivo. Você também pode querer lidar com possíveis erros ou exceções que possam ocorrer durante o processo de escrita. Para se aprofundar neste tópico, confira a documentação oficial sobre manipulação de arquivos em Elixir.

## Veja também

- Documentação oficial sobre manipulação de arquivos em Elixir: https://hexdocs.pm/elixir/File.html
- Artigo da Elixir School sobre manipulação de arquivos: https://elixirschool.com/pt/lessons/advanced/files/
- Tutorial em vídeo sobre escrita de arquivos de texto em Elixir: https://www.youtube.com/watch?v=UoVpQQ69iCg