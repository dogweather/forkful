---
title:    "Elixir: Extraindo subsequências"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma tarefa muito comum na programação. Ao dominar essa habilidade em Elixir, você poderá melhorar sua eficiência e tornar seu código mais elegante.

## Como Fazer

Através da função `String.slice/4`, é possível extrair uma substring de uma string a partir de um índice inicial e final. Por exemplo:

```Elixir
string = "Olá, mundo!"
String.slice(string, 2, -1) #=> "á, mundo"
```

É importante notar que o índice final pode ser negativo, indicando a partir de qual caractere contar a partir do final da string. Outro ponto importante é que os índices em Elixir começam em 0, então o primeiro caractere da string é considerado 0, o segundo 1 e assim por diante.

Além disso, é possível passar como entrada uma lista de índices para a função `String.slice/4` e receber uma lista de substrings correspondentes. Por exemplo:

```Elixir
string = "abcdef"
String.slice(string, [0, 2, 4]) #=> ["a", "c", "e"]
```

## Deep Dive

Para entender melhor como a função `String.slice/4` funciona, é importante saber que em Elixir as strings são representadas como listas de caracteres. Isso significa que cada caractere da string é considerado um elemento da lista. Por exemplo:

```Elixir
string = "abc"
[104, 101, 108, 108, 111] #=> "abc"
```

Com isso em mente, podemos entender que a função `String.slice/4` utiliza os índices para percorrer a lista e retornar os elementos que estão dentro do intervalo fornecido. Isso também justifica o fato da contagem dos índices começar a partir de 0, já que em Elixir as listas também são indexadas a partir de 0.

## Veja também

- [Documentação da função `String.slice/4`](https://hexdocs.pm/elixir/String.html#slice/4)
- [Tutorial sobre como manipular strings em Elixir](https://medium.com/@candland/manipulando-strings-em-elixir-c35c6a3cd9d2)