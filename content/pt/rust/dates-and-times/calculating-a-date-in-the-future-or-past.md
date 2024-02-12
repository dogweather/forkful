---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:32:01.238223-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Calcular uma data no futuro ou passado significa determinar um dia específico antes ou depois de uma data conhecida. Programadores fazem isso para gerenciar eventos, agendamentos, lembretes ou qualquer funcionalidade que dependa do tempo.

## Como Fazer:
```Rust
use chrono::{Duration, Local};

fn main() {
    let hoje = Local::now();
    println!("Hoje: {}", hoje.format("%d/%m/%Y %H:%M"));

    let daqui_a_cinco_dias = hoje + Duration::days(5);
    println!("Daqui a 5 dias: {}", daqui_a_cinco_dias.format("%d/%m/%Y %H:%M"));

    let ha_dez_dias = hoje - Duration::days(10);
    println!("Há 10 dias: {}", ha_dez_dias.format("%d/%m/%Y %H:%M"));
}
```
_saída esperada:_
```
Hoje: 02/04/2023 15:30
Daqui a 5 dias: 07/04/2023 15:30
Há 10 dias: 23/03/2023 15:30
```

## Aprofundamento
A capacidade de calcular datas é crucial desde os primórdios da programação. Antes das bibliotecas especializadas, como a `chrono` em Rust, programadores tinham que lidar manualmente com as complexidades dos calendários e fusos horários. Alternativas modernas incluem o uso de APIs de tempo padrão que já vêm com as linguagens (como `std::time::Duration` em Rust), mas estas podem não ser suficientemente robustas para todos os casos de uso. Na implementação de cálculos de datas, também é essencial considerar questões como anos bissextos, a variação no número de dias por mês e normalizações de fuso horário.

## Veja Também
- Documentação do Chrono para Rust: https://docs.rs/chrono/0.4.19/chrono/
- Rust by Example - Trabalhando com hora e data: https://doc.rust-lang.org/rust-by-example/std_misc/chrono.html
- The Time Crate (uma alternativa ao Chrono): https://docs.rs/time/0.3.9/time/
