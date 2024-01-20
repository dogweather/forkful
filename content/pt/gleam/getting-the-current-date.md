---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:14:24.296722-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Obter a data atual é pegar o momento presente do sistema. Programadores fazem isso para registrar eventos, comparar datas ou simplesmente exibir informações temporais.

## Como Fazer:
```gleam
import gleam/erlang/time
import gleam/io

pub fn main() {
  let data_atual = time.now()
  io.println(data_atual)
}
```
Saída de amostra:
```
{{2023, 2, 18}, {17, 54, 32}}
```

## Aprofundando:
No passado, Gleam herdou suas funções de manipulação de tempo do Erlang, uma linguagem de programação madura e robusta usada em sistemas de telecomunicações. Hoje, Gleam fornece sua própria interface de alto nível para tarefas comuns. Existem alternativas como bibliotecas de terceiros para trabalhar com datas e horas, mas a biblioteca padrão geralmente é suficiente para as necessidades básicas. Por baixo dos panos, a função `time.now()` do Gleam engloba a função do Erlang `:erlang.timestamp()`, que fornece o tempo do sistema operacional em uma tupla com data e hora.

## Veja Também:
- [Gleam stdlib documentation](https://hexdocs.pm/gleam_stdlib/)
- [Erlang's time functions](http://erlang.org/doc/man/os.html)