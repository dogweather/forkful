---
title:    "Elixir: Buscando e substituindo texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Por que usar Elixir para buscar e substituir texto?

Muitos programadores optam por usar a linguagem de programação Elixir por sua eficiência e facilidade de uso. Ao trabalhar com texto, a funcionalidade de busca e substituição é essencial para economizar tempo e aumentar a precisão. Com Elixir, é possível realizar essa tarefa de forma rápida e precisa.

## Como fazer a busca e substituição de texto em Elixir

Para buscar e substituir texto em Elixir, é necessário utilizar a função `String.replace/3`, que recebe três argumentos: a string original, o padrão a ser buscado e o padrão de substituição. Veja um exemplo abaixo:

```Elixir
text = "O Elixir é uma linguagem de programação incrível!"

text = String.replace(text, "Elixir", "Ruby")

IO.puts(text)
```

O código acima irá substituir a palavra "Elixir" por "Ruby" na string original e o output será: "O Ruby é uma linguagem de programação incrível!"

## Deeper Dive: Buscando e substituindo texto com expressões regulares

Em alguns casos, pode ser necessário utilizar expressões regulares para realizar a busca e substituição de texto em Elixir. Para isso, é preciso utilizar a função `Regex.replace/3`. Veja um exemplo:

```Elixir
text = "O número de telefone é (555) 123-4567"

regex = ~r/(\(\d{3}\)) (\d{3})-(\d{4})/

text = Regex.replace(text, regex, "Phone number: \\1 \\2-XXXX")

IO.puts(text)
```

O código acima irá buscar por qualquer número de telefone no formato (555) 123-4567 e substituí-lo pela string "Phone number: (555) 123-XXXX", ocultando os quatro últimos dígitos. O output será: "O número de telefone é Phone number: (555) 123-XXXX".

# Veja também

- [Documentação oficial do Elixir sobre `String.replace/3`](https://hexdocs.pm/elixir/String.html#replace/3)
- [Documentação oficial do Elixir sobre `Regex.replace/3`](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Tutorial sobre expressões regulares em Elixir](https://robots.thoughtbot.com/elixir-and-regular-expressions)