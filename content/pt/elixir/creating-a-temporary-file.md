---
title:                "Elixir: Criando um arquivo temporário"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em Elixir?

Muitas vezes, em nossos projetos de programação, precisamos armazenar informações temporariamente antes de processá-las ou enviá-las para outro lugar. Nesses casos, pode ser útil criar um arquivo temporário para armazenar esses dados de forma organizada e facilmente acessível. Neste artigo, vamos explorar como criar um arquivo temporário em Elixir e como ele pode ser útil em nossos projetos.

## Como fazer

Para criar um arquivo temporário em Elixir, podemos usar a função `Tempfile.open/1` da biblioteca padrão `Tempfile`. Esta função recebe um argumento opcional, que é o nome de prefixo para o arquivo temporário. Se nenhum argumento for fornecido, um prefixo de "elixir_tempfile" será usado por padrão.

Vamos dar uma olhada em um exemplo de código usando `Tempfile.open/1`:

```elixir
file = Tempfile.open("my_tempfile")
IO.puts "Nome do arquivo: #{file.path}"
file.write("Este é um arquivo temporário criado em Elixir!")
file.close()
```

Neste exemplo, usamos o prefixo "my_tempfile" para nosso arquivo temporário e gravamos um texto nele usando a função `write/1`. Em seguida, fechamos o arquivo usando `close/0`. Agora, se quisermos ver o conteúdo do arquivo, podemos abri-lo novamente usando `file.open/2` e ler seu conteúdo.

Se não especificarmos um nome de prefixo, podemos ainda usar a função `file.path` para obter o caminho do arquivo criado. Vamos ver um exemplo sem o argumento do prefixo:

```elixir
file = Tempfile.open()
IO.puts "Nome do arquivo: #{file.path}"
file.write("Este é um arquivo temporário criado em Elixir!")
file.close()
```

Neste caso, nosso arquivo terá um prefixo padrão "elixir_tempfile" e o restante do processo será o mesmo.

## Profundando no assunto

Ao criar um arquivo temporário em Elixir, é importante lembrar que ele é automaticamente excluído quando o processo é encerrado ou quando o arquivo é fechado. Isso pode ser útil, pois não precisamos nos preocupar em excluir manualmente o arquivo temporário. No entanto, se quisermos que o arquivo persista após o término do processo, podemos usar a função `unlink/0` antes de fechar o arquivo.

Ainda na biblioteca padrão `Tempfile`, temos a função `stream!/2` que nos permite criar um arquivo temporário como um fluxo. Isso significa que podemos ler e gravar no arquivo como se fosse um módulo `IO.stream`. Isso pode ser especialmente útil quando precisamos de uma estrutura de dados dinâmica para armazenar nossa informação temporariamente.

Existem também bibliotecas de terceiros que podem ser úteis ao trabalhar com arquivos temporários em Elixir, como [tempfilex](https://github.com/davidjairala/tempfilex), que fornece funções adicionais para gerenciar e manipular arquivos temporários.

# Veja também

- [Documentação oficial da biblioteca Tempfile em Elixir](https://hexdocs.pm/elixir/Tempfile.html)
- [Tempfilex no GitHub](https://github.com/davidjairala/tempfilex)