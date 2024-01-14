---
title:    "Elixir: Extraindo subtrings"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Elixir?

Extrair substrings é um conceito bastante útil na programação em Elixir. Isso permite que você selecione partes específicas de uma string, o que pode ser útil em várias situações, como formatação de dados ou manipulação de texto.

## Como fazer em Elixir?

Extrair substrings em Elixir é muito fácil. Tudo o que você precisa é da função `String.slice/2` e dos parâmetros da posição inicial e final da substring desejada.

```Elixir
#Exemplo 1: Extraindo uma substring de uma string
string = "Olá mundo!"
substring = String.slice(string, 4, 8)
IO.puts(substring)

#Saída: mundo
```

Você também pode usar índices negativos para contar a partir do final da string. E se quiser extrair uma substring até o final da string, basta deixar o segundo parâmetro em branco.

```Elixir
#Exemplo 2: Extraindo uma substring até o final da string
string = "Olá mundo!"
substring = String.slice(string, 4, "")
IO.puts(substring)

#Saída: mundo!
```

## Aprofundando

Além da função `String.slice/2`, Elixir também possui outras funções úteis para extrair substrings de uma string, como `String.split/2` e `String.replace/4`. Essas funções permitem que você selecione substrings com base em critérios específicos, como um caractere delimitador ou um padrão de regex.

Além disso, é importante notar que as strings em Elixir são imutáveis, o que significa que ao extrair uma substring, na verdade, você está criando uma nova string com a substring desejada. Portanto, tomar cuidado com a manipulação excessiva de strings pode afetar a eficiência do código.

## Veja também

- Documentação oficial do Elixir: https://elixir-lang.org/getting-started/string-patterns-and-operations.html
- Tutorial de Elixir: https://elixir-lang.org/getting-started/basic-types.html#binaries-and-strings-extracting-data-from-binaries
- Exemplo de implementação: https://gist.github.com/vipulnsward/41824edbcbf62d780d42f86a27a82ed1