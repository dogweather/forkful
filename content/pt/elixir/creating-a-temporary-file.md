---
title:                "Elixir: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário?

Criar um arquivo temporário pode ser útil em diversas situações, como por exemplo quando precisamos armazenar dados temporários durante a execução de um programa ou quando queremos compartilhar esses dados entre diferentes sessões de um aplicativo.

## Como fazer:

Para criar um arquivo temporário em Elixir, podemos usar o módulo `File` e a função `temp_file/2`. Veja o exemplo abaixo:

```Elixir
{:ok, file_path} = File.temp_file("prefixo", "extensao")
# O primeiro argumento é o prefixo do nome do arquivo e o segundo é a extensão.
IO.puts file_path
# Saída: "/var/folders/d2/zwRl3Q0t3B30G0J+dkBNHE++TIcrbU/temp_1wWecd.tmp"
```

Podemos também especificar um diretório de destino para o arquivo temporário:

```Elixir
# Criando um arquivo temporário no diretório "temp" dentro da pasta atual
{:ok, file_path} = File.temp_file("prefixo", "extensao", "temp/")
```

Ao executar esses exemplos, veremos que o arquivo temporário é criado com um nome único, começando com o prefixo especificado e seguido de caracteres aleatórios. O arquivo também é automaticamente excluído quando o processo que o criou é encerrado.

## Profundidade:

Ao criar um arquivo temporário, é importante se certificar de que o arquivo foi criado com sucesso e de que temos permissão para acessá-lo. Portanto, é recomendado sempre utilizar a função `temp_file/2` dentro de uma cláusula `case` ou `with`, para tratar possíveis erros. Além disso, podemos utilizar a função `File.open/2` para acessar o arquivo e adicionar, ler ou modificar seu conteúdo.

Outra opção é utilizar a biblioteca `Tempfile`, que oferece uma interface mais amigável para trabalhar com arquivos temporários em Elixir, incluindo funções para criar diretórios temporários e definir opções personalizadas para os arquivos.

Para mais informações sobre a criação e utilização de arquivos temporários em Elixir, recomendo consultar a documentação oficial e os links abaixo.

## Veja também:

- Documentação oficial sobre a função `File.temp_file/2` (em inglês): https://hexdocs.pm/elixir/File.html#temp_file/3
- Documentação oficial sobre a biblioteca `Tempfile` (em inglês): https://hexdocs.pm/tempfile/api-reference.html
- Blog post sobre a criação de arquivos temporários com Elixir (em inglês): https://dev.to/andrew_a_bell/tempfile-improving-elixir-s-standard-library-2pip