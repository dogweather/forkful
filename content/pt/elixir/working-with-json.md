---
title:                "Elixir: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-json.md"
---

{{< edit_this_page >}}

##Por que Trabalhar com JSON em Elixir?

O JSON (JavaScript Object Notation) é um formato de dados amplamente utilizado na comunicação entre sistemas web. Ele é uma alternativa leve e eficiente ao formato XML. Em Elixir, trabalhar com JSON pode aumentar a produtividade e facilitar o desenvolvimento de aplicações web.

##Como Fazer:

Para começar a trabalhar com JSON em Elixir, é necessário utilizar a biblioteca "Jason". Primeiramente, é preciso adicioná-la às dependências do seu projeto:

```Elixir
def deps do
  [{:jason, "~> 1.0"}]
end
```

Após a instalação, é necessário fazer a inclusão da biblioteca no seu código:

```Elixir
iex> require Jason
```

Agora você pode facilmente converter dados em formato JSON utilizando a função "encode":

```Elixir
iex> data = %{name: "João", age: 25}
iex> Jason.encode(data)
"{\"name\":\"João\",\"age\":25}"
```

Para transformar dados em formato JSON em Elixir para o tipo de dados da linguagem, basta utilizar a função "decode":

```Elixir
iex> json = "{\"name\":\"Pedro\",\"age\":30}"
iex> Jason.decode(json)
%{"name" => "Pedro", "age" => 30}
```

##Mergulhando Fundo:

Além das funções "encode" e "decode", a biblioteca Jason oferece outras funcionalidades para trabalhar com JSON em Elixir. É possível converter dados em formato JSON para o tipo "Map" utilizando as funções "parse" e "parse!".

```Elixir
iex> json = "{\"fruit\":\"Banana\",\"quantity\":10}"
iex> Jason.parse(json)
%{"fruit" => "Banana", "quantity" => 10}
```

Caso o formato do JSON seja inválido, a função "parse!" retornará um erro ao invés de criar um tipo "Map" vazio.

Também é possível configurar opções adicionais para a conversão de dados utilizando o módulo "Jason.Config". Por exemplo, é possível converter string "null" em Elixir para "nil" adicionando a seguinte configuração:

```Elixir
iex> Jason.Config.validate_null_values()
```

Com essa configuração, a função "parse" irá retornar o valor "nil" caso encontre a string "null" em um objeto JSON.

##Veja Também:

- Página oficial da biblioteca Jason: https://hexdocs.pm/jason/Jason.html
- Tutorial de como trabalhar com JSON em Elixir: https://medium.com/@sathiya9128/working-with-json-in-elixir-eb2ca8b12b08
- Documentação oficial do Elixir sobre JSON: https://elixir-lang.org/getting-started/mapping.html#json