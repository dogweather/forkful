---
title:                "Registro de Logs"
date:                  2024-01-26T01:04:04.285828-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/logging.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Log é essencialmente como registramos o que acontece nos nossos programas. É como ter uma caixa preta; quando as coisas dão errado (e acredite, isso vai acontecer), os logs são inestimáveis para descobrir o que aconteceu, diagnosticar problemas e otimizar o desempenho.

## Como fazer:
No Gleam, você normalmente adicionaria uma biblioteca de log - não há um mecanismo de log dedicado imediatamente disponível. Digamos que estamos usando uma `crate` hipotética chamada `gleam_logger`. Aqui está como você poderia integrá-la:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("O aplicativo está iniciando!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Cálculo bem-sucedido", value)
    Error(err) -> 
      gleam_logger.error("Cálculo falhou", err)
  }
}
```

A saída esperada em seus logs seria algo assim:

```
INFO: O aplicativo está iniciando!
DEBUG: Cálculo bem-sucedido 42
ERROR: Cálculo falhou Razão: Divisão por zero
```

## Mergulho Profundo
A arte do log existe desde os primeiros dias da programação. Operadores de sistema literalmente recebiam logs do computador - garantindo que tudo corresse sem problemas. Avançando no tempo, o log se tornou digital, tornando-se uma parte fundamental do desenvolvimento de software.

Enquanto o Gleam, sendo uma linguagem relativamente nova que tem como alvo o ecossistema da Erlang, não possui uma estrutura de log embutida, você pode aproveitar as instalações de log maduras da Erlang ou outras bibliotecas fornecidas pela comunidade. Cada uma possui diferentes recursos e compensações: algumas podem oferecer log estruturado, outras são mais para saída de texto simples.

Agora, a questão da implementação de uma facilidade de log: É simples? À primeira vista, sim. Mas ao analisar mais a fundo, estamos falando de lidar com concorrência, gargalos de E/S, rotação de log, padronização de formato (pense em JSON para log estruturado), filtragem de níveis e, possivelmente, rastreamento distribuído. Além disso, em um paradigma funcional, geralmente queremos que efeitos colaterais (como o log) sejam tratados de maneira previsível e controlada.

## Veja Também
Aqui é onde você pode encontrar mais sobre os detalhes do log no Gleam e seu ecossistema:
- [Documentação do :logger da Erlang](http://erlang.org/doc/apps/kernel/logger_chapter.html): Como o Gleam compila para Erlang, isso é diretamente aplicável.
- [Documentos da biblioteca padrão do Gleam](https://hexdocs.pm/gleam_stdlib/): Para atualizações sobre quaisquer utilidades de log que possam ser adicionadas.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Uma lista selecionada de recursos, que pode incluir bibliotecas de log conforme elas se tornam disponíveis.
