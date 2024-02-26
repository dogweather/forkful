---
date: 2024-01-20 17:30:55.147423-07:00
description: "Calcular uma data no futuro ou no passado \xE9 determinar um dia espec\xED\
  fico antes ou depois de uma data conhecida. Programadores fazem isso para agendar\u2026"
lastmod: '2024-02-25T18:49:44.134497-07:00'
model: gpt-4-1106-preview
summary: "Calcular uma data no futuro ou no passado \xE9 determinar um dia espec\xED\
  fico antes ou depois de uma data conhecida. Programadores fazem isso para agendar\u2026"
title: Calculando uma data no futuro ou passado
---

{{< edit_this_page >}}

## O que é & por quê?
Calcular uma data no futuro ou no passado é determinar um dia específico antes ou depois de uma data conhecida. Programadores fazem isso para agendar eventos, calcular prazos ou verificar períodos decorridos.

## Como fazer:
```Elm
import Time exposing (Posix)
import Date
import Date.Extra as DateExtra

-- Definindo uma data de partida (Hoje: 1º de abril de 2023)
hoje : Posix
hoje = Date.fromIsoString "2023-04-01" |> Result.withDefault Date.beginning

-- Calculando uma semana no futuro
umaSemanaNoFuturo : Posix
umaSemanaNoFuturo = DateExtra.add DateExtra.Day 7 hoje

-- Calculando uma semana no passado
umaSemanaNoPassado : Posix
umaSemanaNoPassado = DateExtra.add DateExtra.Day -7 hoje

-- Saídas
Date.toIsoString umaSemanaNoFuturo  -- "2023-04-08"
Date.toIsoString umaSemanaNoPassado  -- "2023-03-25"
```

## Mergulho Profundo
Calcular datas no futuro ou no passado é uma necessidade comum em programação desde o princípio dos computadores. Elm, sendo uma linguagem funcional que foca em segurança de tipo e ausência de efeitos colaterais, usa tipos específicos para datas (`Date` e `Posix`) e funções do pacote `Date.Extra` para manipulá-las. Alternativas incluem a manipulação manual dos milissegundos desde a época Unix (Epoch), mas isso pode resultar em erros de cálculo devido a questões como horário de verão e fusos horários. A abordagem de Elm é segura e legível, mas requer a instalação de pacotes adicionais para funcionalidades extras de data, como `justinmimbs/date`.

## Veja também
- Documentação do Elm sobre o tipo `Posix`: https://package.elm-lang.org/packages/elm/time/latest/Time#Posix
- Pacote `justinmimbs/date` para manipulação avançada de datas: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- `elm-community/elm-time` para funções relacionadas ao tempo em geral: https://package.elm-lang.org/packages/elm-community/elm-time/latest/
