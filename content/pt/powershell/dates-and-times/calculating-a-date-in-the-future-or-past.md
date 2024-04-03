---
date: 2024-01-20 17:32:10.482594-07:00
description: "Como Fazer: Vamos direto ao ponto. Aqui est\xE3o alguns exemplos usando\
  \ PowerShell para calcular datas no futuro e no passado."
lastmod: '2024-03-13T22:44:46.810188-06:00'
model: gpt-4-1106-preview
summary: Vamos direto ao ponto.
title: Calculando uma data no futuro ou passado
weight: 26
---

## Como Fazer:
Vamos direto ao ponto. Aqui estão alguns exemplos usando PowerShell para calcular datas no futuro e no passado.

```PowerShell
# Adicionar 10 dias à data atual
$daquiADezDias = (Get-Date).AddDays(10)
$daquiADezDias

# Resultado esperado (exemplo, dependendo do dia de execução):
# quarta-feira, 12 de abril de 2023 00:00:00

# Subtrair 30 dias da data atual
$trintaDiasAtras = (Get-Date).AddDays(-30)
$trintaDiasAtras

# Resultado esperado (exemplo, dependendo do dia de execução):
# sexta-feira, 3 de março de 2023 00:00:00

# Adicionar 3 meses à data atual
$emTresMeses = (Get-Date).AddMonths(3)
$emTresMeses

# Resultado esperado (exemplo, dependendo do dia de execução):
# sábado, 3 de junho de 2023 00:00:00
```

## Aprofundando
Historicamente, a manipulação de datas em programação sempre foi um pouco complicada devido a vários fatores, como fusos horários, anos bissextos e diferentes calendários. O PowerShell, ao longo do tempo, simplificou bastante essa tarefa com seus comandos intuitivos e flexíveis.

Existem alternativas a considerar. Por exemplo, para tarefas mais complexas de agendamento, muitos preferem utilizar o serviço Cron em sistemas Unix-like. Em .NET, a biblioteca `DateTime` oferece funcionalidades semelhantes que o PowerShell também aproveita.

Detalhes de implementação, como a formatação de saída, podem ser personalizados. O PowerShell permite a personalização do formato de data e hora usando o método `ToString()` com parâmetros específicos, o que é muito útil dependendo do contexto cultural ou os requerimentos do sistema onde está sendo utilizado.

## Veja Também
- [StackOverflow: Operações com datas em PowerShell](https://stackoverflow.com/questions/tagged/powershell+date)
