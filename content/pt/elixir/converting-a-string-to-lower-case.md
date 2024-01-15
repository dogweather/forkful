---
title:                "Convertendo uma string para minúsculas"
html_title:           "Elixir: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Porquê

Convertendo uma string para letras minúsculas é um processo comum na maioria das linguagens de programação. Isso é útil para padronizar strings e facilitar a comparação de dados.

## Como Fazer

Para converter uma string para letras minúsculas em Elixir, podemos usar a função `String.downcase/1`. Veja um exemplo abaixo:

```Elixir
string = "Elixir é uma linguagem de programação"
lowercase_string = String.downcase(string) 

IO.puts lowercase_string 

# saída: elixir é uma linguagem de programação 
```

A função `String.downcase/1` recebe um argumento de uma string e retorna uma nova string com todas as letras em minúsculo. Isso significa que a string original permanece inalterada e a nova string é retornada. 

Podemos também usar o operador de pipe `|>` para tornar o código mais legível:

```Elixir 
string 
|> String.downcase() 
|> IO.puts() 

# saída: elixir é uma linguagem de programação 
```

## Mergulho Profundo

Ao trabalhar com strings em Elixir, é importante entender a diferença entre strings comuns e binários. Strings em Elixir são representadas internamente como listas de caracteres Unicode, enquanto binários são sequências de bytes. Isso significa que converter uma string para letras minúsculas pode ser computacionalmente pesado, pois cada caractere deve ser verificado e convertido individualmente.

No entanto, se estamos lidando com uma grande quantidade de dados ou operando em tempo real, podemos optar por utilizar binários para melhorar o desempenho. Para isso, podemos usar a função `String.to_charlist/1` que converte uma string em uma lista de inteiros representando os códigos Unicode de cada caractere.

```Elixir 
string = "Elixir é uma linguagem de programação"
charlist = string 
|> String.to_charlist() 

IO.puts charlist 

# saída: [69, 108, 105, 120, 105, 114, 32, 233, 32, 117, 109, 97, 32, 108, 
# 105, 110, 103, 117, 97, 103, 101, 109, 32, 100, 101, 32, 
# 112, 114, 111, 103, 114, 97, 109, 97, 231, 227, 111] 
```

Em seguida, podemos usar a função `Enum.map/2` para iterar sobre a lista de códigos e aplicar a função `:unicode.to_lowercase/1` que converte cada código para uma letra minúscula. Em seguida, podemos usar a função `to_string/1` para converter a lista de inteiros em uma string novamente:

```Elixir 
string = string 
|> String.to_charlist() 
|> Enum.map(&(:unicode.to_lowercase(&1))) 
|> to_string() 

IO.puts string 

# saída: elixir é uma linguagem de programação 
```

Tenha em mente que esse método é mais eficiente em termos de desempenho, mas pode ter algumas limitações ao lidar com caracteres especiais ou acentuados. Portanto, é importante testar e garantir que funcione corretamente para o seu caso de uso específico.

## Veja Também

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Funções relacionadas à codificação em Unicode em Elixir](https://hexdocs.pm/elixir/Unicode.html)