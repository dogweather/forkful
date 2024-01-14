---
title:                "Rust: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Por que
Se você está lendo este post, é porque tem interesse em aprender mais sobre programação em Rust. Uma das coisas mais básicas que você pode fazer em qualquer linguagem de programação é obter a data atual. Isso pode parecer simples, mas pode ser um bom ponto de partida para entender melhor como a linguagem funciona.

##Como fazer
Para obter a data atual em Rust, você pode usar a função `now()` do pacote externo `chrono`. Primeiro, você precisa adicionar essa dependência ao seu projeto. Você pode usar o gerenciador de pacotes `Cargo` para fazer isso:

```
Rust
[dependências]
chrono = "0.4"
```

Agora, dentro da sua função `main`, você pode chamar a função `now()` passando como parâmetro um `TimeZone`. Em seguida, você pode formatar a data de acordo com o seu formato preferido. Aqui está um exemplo de código:

```
Rust
use chrono::{Datelike, Timelike, Local};

fn main() {
    let now = Local::now(); // obtém a data atual
    let year = now.year(); // obtém o ano atual
    let month = now.month(); // obtém o mês atual
    let day = now.day(); // obtém o dia atual
    let hour = now.hour(); // obtém a hora atual
    let minute = now.minute(); // obtém o minuto atual
    let second = now.second(); // obtém o segundo atual

    println!("A data atual é: {}/{}/{} às {}:{}:{}", day, month, year, hour, minute, second);
}
```

O resultado desse código seria:
```
A data atual é: 2/5/2021 às 10:30:00
```

##Deep Dive
A função `now()` retorna um objeto `DateTime` que armazena informações sobre a data e hora atuais. Você pode usar os métodos disponíveis para obter informações específicas, como o exemplo mostrado acima. Além disso, a estrutura `DateTime` também possui métodos para comparar datas, converter fusos horários e muito mais. Se você quiser aprender mais sobre as funções disponíveis, pode acessar a documentação oficial do `chrono`.

##Veja também
- Documentação oficial do `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Tutorial básico sobre Rust: https://www.rust-lang.org/pt-BR/learn
- Comunidade Rust Brasil: https://forum.rustbr.org/