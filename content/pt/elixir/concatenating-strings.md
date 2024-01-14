---
title:                "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em Elixir? 

Concatenar strings é uma prática comum em linguagens de programação, e o Elixir não é diferente. Ao unir duas ou mais strings, podemos criar uma nova string com informações combinadas de diferentes fontes, o que pode ser útil em diversas situações durante o desenvolvimento de um programa. Neste artigo, veremos como realizar concatenações de strings em Elixir e porque isso pode ser útil em sua jornada como desenvolvedor. 

## Como fazer em Elixir 

A concatenação de strings é feita utilizando o operador `<>`, que une duas strings em uma nova string. Vejamos um exemplo simples: 

```Elixir 
"Olá " <> "mundo!" 
```

O código acima resultará na saída `"Olá mundo!"`. O operador `<>` pode ser usado com variáveis, permitindo que combinemos informações de diferentes fontes em uma única string. Vejamos um exemplo mais complexo: 

```Elixir 
nome = "João" 
sobrenome = "Silva" 

"Seja bem-vindo, " <> nome <> " " <> sobrenome <> "!" 
```

Neste caso, a saída será `"Seja bem-vindo, João Silva!"`, mostrando como podemos utilizar variáveis em uma concatenação de strings. 

## Aprofundando 

É importante lembrar que o uso excessivo de concatenções pode acarretar em problemas de performance, pois o Elixir irá criar uma nova string a cada concatenação. Por isso, é recomendado utilizar a função `Enum.join/2` quando precisamos unir muitas strings em uma única. 

Além disso, é possível adicionar formatação e transformação de strings durante a concatenação utilizando o operador `<>`. Vejamos alguns exemplos: 

```Elixir 
"Preço: " <> 50 |> Integer.to_string <> " reais" 
```

Nesta concatenação, a string `50` é convertida em um inteiro e adicionada à string `"Preço: "`, resultando em `"Preço: 50 reais"`. 

```Elixir 
"Texto em maiúsculo: " <> "olá" |> String.upcase 
```

Neste exemplo, a string `"olá"` é transformada em maiúsculas utilizando a função `String.upcase` e então concatenada com a string anterior, resultando em `"Texto em maiúsculo: OLÁ"`. 

## Veja também 

- Documentação oficial sobre strings em Elixir: https://hexdocs.pm/elixir/String.html 
- Artigo sobre o uso excessivo de concatenações em Elixir: https://www.cultureamp.com/engineering/elixir-string-performance-problem/ 
- Vídeo explicando mais sobre concatenações em Elixir (em inglês): https://www.youtube.com/watch?v=ZmhwY7Z-YbA

******