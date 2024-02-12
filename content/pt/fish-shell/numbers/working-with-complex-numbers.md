---
title:                "Trabalhando com números complexos"
aliases:
- /pt/fish-shell/working-with-complex-numbers.md
date:                  2024-01-26T04:40:05.191192-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Números complexos expandem a ideia de linhas numéricas unidimensionais para um plano complexo bidimensional. Programadores os usam em campos como engenharia, física e gráficos para cálculos que requerem dois componentes, como sinais ou rotações.

## Como fazer:
No Fish, lidamos com números complexos usando `math` com partes reais e imaginárias. Aqui está um ponto de partida:

```fish
# Adicionar dois números complexos (3+4i) e (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Saída: 8+6i

# Multiplicar dois números complexos (1+2i) e (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Saída: -5+10i
```

Se você precisar elevar um número complexo a uma potência ou obter sua forma exponencial:

```fish
# Quadrado de (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Saída: -5+12i

# Exponencial de (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Saída: -0.41615+0.9093i
```

## Aprofundamento
O suporte do Fish Shell para números complexos é relativamente novo, iniciando por volta da versão 3.1.0. Antes disso, as pessoas poderiam ter usado `bc` ou recorrido a ferramentas externas como Python para matemática complexa.

Alternativas à matemática do Fish incluem bibliotecas numéricas especializadas ou linguagens como MATLAB, Python com NumPy ou até mesmo C++ com a Standard Library. No entanto, estas podem ser exageradas para cálculos rápidos em shell.

O suporte a números complexos do Fish está integrado ao seu comando interno `math`, aproveitando a libcalc. Isso significa que você não precisa instalar ferramentas extras para operações básicas.

No entanto, o Fish não é projetado para cálculos matemáticos intensivos. Sua capacidade matemática é conveniente para cálculos rápidos ou scripts onde números complexos entram em jogo, mas considere ferramentas mais robustas para tarefas intensivas.

## Veja Também
- Documentação do shell Fish para matemática: https://fishshell.com/docs/current/commands.html#math
- NumPy para Python, uma alternativa popular: https://numpy.org/
- Um olhar mais aprofundado sobre números complexos: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
