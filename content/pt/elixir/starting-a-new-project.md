---
title:                "Elixir: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto em Elixir?

Se você está procurando uma linguagem de programação moderna e eficiente para iniciar um novo projeto, Elixir é uma ótima opção. Com sua sintaxe concisa e funcional, sua escalabilidade e tolerância a falhas incorporadas, e sua capacidade de executar em ambientes de produção sem interrupções, Elixir é uma escolha inteligente para qualquer desenvolvedor.

## Como Fazer

Para começar a programar em Elixir, é importante entender alguns conceitos básicos. Uma das principais estruturas utilizadas em Elixir são as funções, que podem ser definidas da seguinte maneira:

```Elixir
def soma(a, b) do
  a + b
end
```

Isso define uma função chamada "soma" que recebe dois parâmetros e retorna a soma deles. Para executar essa função, utilizamos o seguinte comando:

```Elixir
soma(2, 3)
```

O resultado seria 5.

Outro recurso interessante é o "pipe operator", que permite encadear funções de forma mais fácil e legível. Por exemplo:

```Elixir
1..10
|> Enum.map(fn x -> x * 2 end)
|> Enum.filter(fn x -> rem(x, 3) == 0 end)
|> Enum.reduce(0, fn x, acc -> x + acc end)
```

Isso irá gerar uma lista com os números pares entre 1 e 10, filtrando apenas os que são divisíveis por 3 e, em seguida, somando-os. O resultado seria 18.

Além disso, é importante destacar a importância dos testes em Elixir. Com a estrutura de testes integrada na linguagem, é possível garantir que o código esteja funcionando corretamente antes de lançá-lo em produção.

## Aprofundando

Ao iniciar um novo projeto em Elixir, é importante se familiarizar com o sistema de construção de projetos da linguagem, chamado Mix. Ele fornece ferramentas para criar, testar e executar projetos de forma eficiente.

Outro aspecto importante é a utilização do OTP (Open Telecom Platform), um conjunto de bibliotecas e ferramentas que permitem a criação de sistemas altamente escaláveis e tolerantes a falhas. Com as ferramentas do OTP, é possível construir aplicativos que se adaptem às necessidades e demandas de um ambiente de produção.

## Veja também

- [Site oficial do Elixir](https://elixir-lang.org/)
- [Documentação da linguagem](https://elixir-lang.org/docs.html)
- [Blog oficial do Elixir](https://elixir-lang.org/blog/)
- [Vídeos de tutoriais do Elixir](https://www.youtube.com/playlist?list=PLGLfVvz_LVvSVywel3Y_qyz3NJZaE1qOg)