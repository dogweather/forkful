---
title:                "Gleam: Obtendo a data atual"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que precisamos da data atual em programação?

Pensar em códigos e algoritmos é como construir uma máquina do tempo. Para garantir que essa máquina funcione corretamente, precisamos de um elemento fundamental: a data atual. Sem ela, nossos códigos não saberiam o momento em que estão sendo executados, tornando-os menos eficientes ou até mesmo inúteis. Neste artigo, vamos explorar como obter a data atual usando Gleam e como isso pode ser útil em nossos projetos de programação.

## Como obter a data atual com Gleam

Em Gleam, há uma função específica para obter a data atual: `Time.now()`. Essa função retorna um valor do tipo `Time`, que contém informações sobre a data e hora atuais. Para utilizar essa função, primeiro precisamos importar o módulo `gleam/time` em nosso código:

```Gleam
import gleam/time

pub fn main() {
  let current_date = gleam/time.now()
  io.println(date)
}
```

Ao executar esse código, veremos o seguinte resultado:

```
1956-01-31 23:00:00 UTC
```

Podemos também formatar a data para que ela seja mostrada de maneira mais amigável, utilizando a função `gleam/time.format()` e passando o formato desejado como parâmetro:

```Gleam
pub fn main() {
  let current_date = gleam/time.now() |> gleam/time.format("%d/%m/%Y - %H:%M:%S")
  io.println(date)
}
```

O código acima irá retornar a data atual no formato "DD/MM/AAAA - HH:MM:SS", por exemplo:

```
31/01/2021 - 18:30:00
```

Isso pode ser útil para mostrar a data em diferentes idiomas ou formatos, de acordo com as necessidades do nosso projeto.

## Uma análise mais profunda sobre a obtenção da data atual

Ao utilizar a função `Time.now()` em Gleam, estamos na verdade chamando a função `os:timestamp()` do sistema operacional, que nos retorna a data e hora atual do servidor em que nosso código está sendo executado. Isso significa que, se nosso servidor estiver em um fuso horário diferente do nosso, a data retornada também será diferente. Por isso, é importante estar ciente dessas diferenças ao utilizar essa função.

Além disso, a função `os:timestamp()` retorna a data e hora em um formato específico, geralmente no formato de segundos desde 1º de janeiro de 1970, conhecido como Unix Epoch Time. Por isso, a utilização da função `gleam/time.format()` é essencial para formatar a data para um formato mais legível e compreensível.

## Veja também

- [Documentação oficial do módulo `gleam/time`](https://gleam.run/modules/gleam_stdlib/time.html)
- [Artigo sobre o funcionamento do Unix Epoch Time](https://www.unixtimestamp.com/)
- [Tutorial sobre a formatação de datas no Gleam](https://medium.com/gleam-lang/how-to-format-dates-in-gleam-94f1dae14165)