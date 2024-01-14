---
title:    "Rust: Convertendo uma data em uma string"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Ao trabalhar com datas em uma linguagem de programação, é comum a necessidade de converter a data para uma string para exibi-la de forma legível para o usuário ou para fins de armazenamento. Neste artigo, vamos explorar como fazer isso em Rust.

## Como fazer

Para converter uma data em uma string em Rust, primeiro precisamos importar o módulo "chrono", responsável por manipular datas e horários. Podemos fazer isso adicionando a seguinte linha ao início do código:

```Rust
use chrono::prelude::*;
```

Agora, vamos criar um objeto do tipo DateTime que representará a data que queremos converter. Por exemplo, para obter a data e hora atual, usamos o seguinte código:

```Rust
let data_hora = Local::now();
```

Em seguida, usamos o método "format" para definir o formato em que queremos converter a data. Por exemplo, para exibir apenas a data no formato "dia/mês/ano", usamos o seguinte código:

```Rust
let data_em_string = data_hora.format("%d/%m/%Y").to_string();
```

E pronto! Agora, podemos imprimir a string "data_em_string" e veremos a data atual no formato desejado.

## Mergulho profundo

O módulo "chrono" oferece muitas opções para personalizar o formato das datas. Por exemplo, podemos adicionar o horário e o fuso horário à string, ou escolher entre as datas no formato de 12 ou 24 horas. Também é possível alterar a língua utilizada na exibição dos meses e dias da semana.

Além disso, para casos mais complexos, podemos utilizar bibliotecas externas, como o "date_time", que oferece ainda mais funcionalidades para trabalhar com datas em Rust.

## Veja também

- Documentação do módulo "chrono": https://docs.rs/crate/chrono/0.4.19
- Biblioteca "date_time": https://crates.io/crates/date_time