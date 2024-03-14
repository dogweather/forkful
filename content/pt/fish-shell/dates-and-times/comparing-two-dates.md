---
date: 2024-01-20 17:33:03.469034-07:00
description: "Comparar duas datas significa verificar a diferen\xE7a ou a rela\xE7\
  \xE3o temporal entre elas, como qual vem antes ou quanto tempo se passou entre uma\
  \ e outra.\u2026"
lastmod: '2024-03-13T22:44:47.018237-06:00'
model: gpt-4-1106-preview
summary: "Comparar duas datas significa verificar a diferen\xE7a ou a rela\xE7\xE3\
  o temporal entre elas, como qual vem antes ou quanto tempo se passou entre uma e\
  \ outra.\u2026"
title: Comparando duas datas
---

{{< edit_this_page >}}

## O Que & Porquê?
Comparar duas datas significa verificar a diferença ou a relação temporal entre elas, como qual vem antes ou quanto tempo se passou entre uma e outra. Programadores fazem isso para, por exemplo, validar prazos, ordenar eventos ou calcular períodos de tempo em aplicações.

## Como Fazer:
```Fish Shell
# Defina as datas em formato AAAA-MM-DD
set data1 2023-03-10
set data2 2023-03-15

# Converta as datas para segundos desde a época (Epoch)
set seg1 (date -ud $data1 +%s)
set seg2 (date -ud $data2 +%s)

# Calcule a diferença em segundos
set diff (math $seg2 - $seg1)

# Converta segundos em dias
set dias (math $diff / 86400)
echo "A diferença é de $dias dias."
```
Saída esperada:
```
A diferença é de 5 dias.
```

## Mergulho Profundo
Comparar datas é um desafio clássico em programação devido aos diferentes calendários e unidades de medida de tempo. Por isso, a partir do UNIX e sua ideia de medir o tempo em segundos desde a "Epoch" (1 de Janeiro de 1970), essa se tornou uma base comum para comparação de tempo. Alternativas ao uso direto de segundos incluem funções e bibliotecas especializadas, como a DateTime no Python ou a Chrono no Rust. No Fish, utilizamos funções externas como `date` para manipulação de datas, pois o shell por si só não possui ferramentas incorporadas para isso. A precisão e validade da comparação de datas dependem da correta manipulação de zonas horárias e formatos date-time.

## Veja Também
- Documentação `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Tutorial Fish Shell: https://fishshell.com/docs/current/tutorial.html
- “Design of Time: Understanding UNIX Time”: https://www.eecis.udel.edu/~mills/y2k.html
- Fórum de ajuda para Fish Shell: https://fishshell.com/docs/current/index.html#help
