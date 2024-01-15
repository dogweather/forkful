---
title:                "Obtendo a data atual."
html_title:           "Rust: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que
A obtenção da data e hora atuais é uma tarefa comum em muitos projetos de programação. Isso pode ser usado para registrar eventos, medir o tempo de execução de um programa e várias outras aplicações.

## Como Fazer
Em Rust, podemos obter a data e hora atuais usando a biblioteca padrão `time`. Começamos importando a biblioteca:

```rust
use std::time::{SystemTime, UNIX_EPOCH};
```

Em seguida, podemos usar o método `now()` para obter a data e hora do sistema:

```rust
let current_time = SystemTime::now();
```

Agora, podemos usar o método `duration_since()` para encontrar a diferença entre a data e hora atual e o ano de referência, 1970:

```rust
let time_since_epoch = current_time.duration_since(UNIX_EPOCH).expect("Failed to get current time");
```

Por fim, podemos imprimir a data e hora atuais usando o método `as_secs()` para converter o tempo em segundos e, em seguida, usando a função `println!()`:

```rust
println!("A data e hora atuais são: {:?}", time_since_epoch.as_secs());
```

O caractere `?` é usado para indicar que estamos lidando com um valor opcional e, em caso de falha em obter a data e hora atual, a mensagem de erro fornecida será impressa. Aqui está um exemplo de saída:

```
A data e hora atuais são: 1594848416
```

## Mergulho Profundo
A razão pela qual usamos a data de referência, 1970, é porque esse é o início do tempo UNIX, um sistema de rastreamento de tempo amplamente usado em sistemas operacionais baseados em UNIX. Na verdade, em sistemas baseados em UNIX, o número de segundos desde o início do tempo UNIX é usado para representar a data e hora atual.

A estrutura de dados usada para a nossa data e hora atual é `Duration`, que armazena o tempo em segundos, minutos, horas, dias, semanas, etc. Cada uma dessas unidades é armazenada como um número inteiro de 64 bits, permitindo que os dados sejam representados com precisão e abrangência. O método `as_secs()` converte essa estrutura de dados em segundos para facilitar a exibição.

## Veja Também
- [Documentação da Biblioteca `time`](https://doc.rust-lang.org/std/time/)
- [Tutorial do Rust: Data e Hora](https://stevedonovan.github.io/rust-gentle-intro/6-dates.html)
- [Guia da Comunidade do Rust](https://www.rust-lang.org/community)