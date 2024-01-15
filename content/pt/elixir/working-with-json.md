---
title:                "Lidando com JSON"
html_title:           "Elixir: Lidando com JSON"
simple_title:         "Lidando com JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON em Elixir?

Trabalhar com JSON é essencial para qualquer desenvolvedor que queira criar aplicações web modernas e interativas. JSON é a maneira mais comum de enviar e receber dados entre os clientes e os servidores, tornando-se uma importante habilidade no desenvolvimento de software.

## Como funciona?

Para manipular dados em JSON em Elixir, é necessário utilizar a biblioteca `Jason`. Primeiramente, é preciso instalar a biblioteca no seu projeto, adicionando a seguinte linha na seção "deps" do seu arquivo `mix.exs`:

```Elixir
{:jason, "~> 1.2"}
```

Em seguida, no seu módulo, você pode utilizar a função `Jason.decode/1` para converter uma string JSON em um mapa Elixir:

```Elixir
json = "{\"name\": \"João\", \"age\": 30}"
Jason.decode(json)
# => %{"name" => "João", "age" => 30}
```
Da mesma forma, a função `Jason.encode/1` pode ser utilizada para converter um mapa Elixir em uma string JSON:

```Elixir
data = %{"name" => "Maria", "age" => 25}
Jason.encode(data)
# => "{\"name\":\"Maria\",\"age\":25}"
```

## Aprofundando-se em JSON em Elixir

Além de converter dados, a biblioteca `Jason` também possui funções úteis para trabalhar com JSON em Elixir. Por exemplo, a função `Jason.parse!/1` pode ser utilizada para ler um arquivo JSON diretamente, sem ter que primeiro convertê-lo em uma string:

```Elixir
Jason.parse!("example.json")
# => %{"name" => "Ana", "age" => 28}
```

Além disso, a biblioteca permite que você trabalhe com chaves atom em vez de strings, o que torna o código mais legível e eficiente:

```Elixir
Jason.decode("{\"name\": \"Pedro\", \"age\": 35}", as: :atoms)
# => %{name: "Pedro", age: 35}
```

Existem também outras opções para personalizar o comportamento do `Jason`, como alterar a estratégia de conversão de chaves e valores ou definir o limite máximo de profundidade para a conversão.

## Veja também

- Documentação oficial da biblioteca `Jason`: https://hexdocs.pm/jason/api-reference.html
- Artigo sobre como trabalhar com JSON em Elixir: https://www.ithaka.dev/blog/trabalhando-com-json-em-elixir/