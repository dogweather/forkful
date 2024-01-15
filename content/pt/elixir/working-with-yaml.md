---
title:                "Trabalhando com yaml"
html_title:           "Elixir: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML (YAML Ain't Markup Language) é uma linguagem de marcação de dados que pode ser utilizada para estruturar e armazenar informações de forma legível para humanos e máquinas. É especialmente útil para configurar e gerenciar projetos de software. Ao aprender a trabalhar com YAML, você conseguirá organizar melhor seus dados e otimizar seu fluxo de trabalho.

## Como fazer?

Para começar a trabalhar com YAML em Elixir, primeiro precisamos importar o módulo YAML do pacote `:yaml_elixir`.

```
iex> import YAML
```

Em seguida, podemos usar a função `decode/1` para converter um arquivo YAML em uma estrutura de dados que possamos manipular em nosso código.

```
iex> data = decode(File.read!("arquivo.yml"))
%{
  "nome" => "João",
  "idade" => 30,
  "hobbies" => ["viajar", "ler", "cozinhar"]
}
```

Podemos acessar os valores do arquivo YAML como um mapa:

```
iex> data["nome"]
"João"
```

E também podemos adicionar novas entradas ou alterar valores existentes:

```
iex> data["país"] = "Brasil"
%{
  "nome" => "João",
  "idade" => 30,
  "hobbies" => ["viajar", "ler", "cozinhar"],
  "país" => "Brasil"
}

iex> data["idade"] = 31
%{
  "nome" => "João",
  "idade" => 31,
  "hobbies" => ["viajar", "ler", "cozinhar"],
  "país" => "Brasil"
}
```

Para transformar uma estrutura de dados em um arquivo YAML, podemos usar a função `encode/2`:

```
iex> encode(data, "arquivo.yml")
:ok
```

Com isso, teremos um arquivo `arquivo.yml` contendo nossas alterações.

## Mergulho profundo

O pacote `:yaml_elixir` também oferece outras funções úteis para trabalhar com YAML, como `load/1` para carregar um arquivo YAML diretamente em uma variável, `dump/2` para converter diretamente uma estrutura de dados em YAML, e `encode!/1` e `decode!/1`, que funcionam como as funções mencionadas anteriormente, mas retornam erros se algo der errado durante o processo.

Também é possível personalizar o processo de codificação e decodificação de dados YAML fornecendo opções adicionais para as funções `encode/2` e `decode/2`. Isso pode ser útil especialmente para lidar com tipos personalizados, como datas ou enumerações.

## Veja também

- [Documentação do pacote YAML Elixir](https://hexdocs.pm/yaml_elixir/YAML.html)
- [Artigo sobre YAML no Medium](https://medium.com/@alexandreservian/um-guia-prático-para-yaml-8786d5a72a3f)
- [Tutorial sobre YAML em Elixir no YouTube](https://www.youtube.com/watch?v=w9u_vkzQHvk)