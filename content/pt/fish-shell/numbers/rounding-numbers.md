---
date: 2024-01-26 03:44:06.987667-07:00
description: "Arredondar n\xFAmeros consiste em eliminar casas decimais para simplificar\
  \ seus dados ou adequ\xE1-los a formatos espec\xEDficos. Programadores fazem isso\
  \ para\u2026"
lastmod: '2024-03-13T22:44:46.998899-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros consiste em eliminar casas decimais para simplificar\
  \ seus dados ou adequ\xE1-los a formatos espec\xEDficos. Programadores fazem isso\
  \ para\u2026"
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## O que & Por quê?
Arredondar números consiste em eliminar casas decimais para simplificar seus dados ou adequá-los a formatos específicos. Programadores fazem isso para exibir dados de forma amigável ao usuário, armazenar de maneira eficiente, ou quando a precisão decimal não é um problema.

## Como fazer:
No Fish, o arredondamento de números depende do comando `math`. Use `math -s0` para arredondar para o inteiro mais próximo.

```fish
# Arredondar para cima
echo (math -s0 "4.7")
# Saída: 5

# Arredondar para baixo
echo (math -s0 "4.3")
# Saída: 4

# Arredondar para duas casas decimais
echo (math -s2 "4.5678")
# Saída: 4.57

# Arredondar número negativo
echo (math -s0 "-2.5")
# Saída: -3
```

## Aprofundando
Historicamente, o arredondamento de números era feito de maneira mais manual ou com ferramentas externas, mas em shells modernos como o Fish, isso está incorporado às utilidades internas. A abordagem do Fish usando o comando `math` simplifica as coisas em comparação com shells mais antigos. Alternativas em outros ambientes de programação variam; linguagens como Python usam funções como `round()`, enquanto o Bash pode exigir expressões mais complexas ou a utilidade `bc`. A implementação de arredondamento do Fish simplifica a criação de scripts ao manter a matemática dentro do ambiente do shell, em vez de invocar outras ferramentas ou linguagens.

## Veja também
- Documentação do Fish para o comando `math`: https://fishshell.com/docs/current/cmds/math.html
- Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754): https://ieeexplore.ieee.org/document/4610935
