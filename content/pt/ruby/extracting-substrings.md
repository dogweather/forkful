---
title:    "Ruby: Extraindo subcadeias"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que é importante extrair substrings?

Extrair substrings é uma tarefa comum em programação, especialmente em linguagens como Ruby. Isso porque essa funcionalidade permite que os desenvolvedores manipulem strings de maneira mais eficiente e precisa, destacando apenas as partes necessárias. Além disso, a extração de substrings é essencial para a validação e formatação de dados.

## Como fazer

A extração de substrings em Ruby é simples e pode ser feita de várias maneiras. Vamos dar uma olhada em algumas delas.

```Ruby
# Exemplo 1: Usando [] com um índice e tamanho
string = "Olá, mundo!"
p string[0, 4] # output: "Olá,"
p string[5..-1] # output: "mundo!"

# Exemplo 2: Usando [] com um intervalo
string = "123456789"
p string[1..3] # output: "234"

# Exemplo 3: Usando slice com índice e tamanho
string = "Eu amo programar"
p string.slice(3, 6) # output: "amo pr"

# Exemplo 4: Usando slice com um intervalo
string = "Ruby é incrível!"
p string.slice(0..3) # output: "Ruby"
```

Como você pode ver, é possível extrair substrings usando índices e tamanhos específicos ou mesmo intervalos. Além disso, a função `slice` também pode ser usada para o mesmo propósito.

## Aprofundando

A extração de substrings pode ser ainda mais aprimorada com a utilização de expressões regulares, que permitem a busca e substituição de padrões em uma string. Por exemplo, se você quiser extrair apenas as letras de uma string, pode usar a seguinte expressão regular: `/[a-zA-Z]/`. Isso retornará apenas as letras maiúsculas e minúsculas da string original.

Além disso, é possível combinar diferentes métodos de extração de substrings para obter resultados ainda mais precisos. Por exemplo, você pode usar a função `scan` para extrair todas as ocorrências de um padrão específico em uma string e depois usar a função `join` para unir esses resultados em uma única string.

## Veja também

- [Documentação oficial do Ruby sobre a extração de substrings](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [Tutorial sobre expressões regulares em Ruby](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-ruby)