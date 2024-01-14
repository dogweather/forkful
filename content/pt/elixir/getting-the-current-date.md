---
title:    "Elixir: Obtendo a data atual."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que usar Elixir para obter a data atual?

Elixir é uma linguagem de programação funcional e dinâmica que tem ganhado popularidade devido à sua simplicidade e escalabilidade. Uma das tarefas comuns em programação é obter a data atual e, com Elixir, esse processo pode ser feito de forma rápida e eficiente. Neste artigo, vamos explorar como usar Elixir para obter a data atual e como isso pode ser útil em seus projetos.

## Como fazer:

Para obter a data atual em Elixir, podemos usar a função ```Elixir Date.utc_today()```, que retorna uma estrutura de data no formato "ano-mês-dia". Podemos atribuir essa função a uma variável e, em seguida, acessar os diferentes componentes da data, como mostrado no exemplo abaixo:

```Elixir
hoje = Date.utc_today()
# => ~D[ano: 2021, mes: 07, dia: 06]

hoje.ano
# => 2021

hoje.mes
# => 07

hoje.dia
# => 06
```

Também podemos criar uma data específica usando a função ```Elixir Date.from_erl()```. Esta função recebe uma data no formato Erlang e a converte para o formato de data Elixir. Por exemplo, para criar uma data para o dia 25 de dezembro de 2021, podemos fazer o seguinte:

```Elixir
natal = Date.from_erl({2021, 12, 25})
# => ~D[ano: 2021, mes: 12, dia: 25]
```

Além disso, Elixir também possui a biblioteca ```Elixir Timex```, que oferece uma variedade de funções e ferramentas para lidar com datas e horários. Ela pode ser instalada facilmente com o comando ```mix deps.get``` e importada no seu arquivo de código.

## Profundidade:

Além das funções mencionadas acima, Elixir também possui outras ferramentas poderosas para lidar com datas. Por exemplo, a função ```Elixir DateTime.utc_now()``` retorna a data e hora atual no formato ```{data, hora}```, permitindo que você acesse tanto a data quanto a hora em um único objeto.

Além disso, é possível converter a estrutura de data para outros formatos, como ISO 8601, usando a função ```Elixir Date.to_iso/1```. Isso pode ser útil para fins de armazenamento e compartilhamento de data em diferentes sistemas.

Em resumo, Elixir possui uma variedade de funções e bibliotecas que facilitam a obtenção e manipulação de datas, tornando esse processo simples e eficiente.

## Veja também:

- [Documentação oficial sobre a biblioteca Timex](https://hexdocs.pm/timex/readme.html)
- [Guia completo sobre manipulação de datas em Elixir](http://culttt.com/2017/01/02/powerful-manipulation-dates-times-elixir/)
- [Tutorial sobre como usar a biblioteca Calendar para trabalhar com datas em Elixir](https://spin.atomicobject.com/2018/05/28/date-time-management-elixir/)

Conteúdos relacionados para continuar aprendendo sobre Elixir e suas funcionalidades.