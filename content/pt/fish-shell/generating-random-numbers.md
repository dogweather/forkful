---
title:                "Geração de números aleatórios"
html_title:           "Fish Shell: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que e por que?

Gerar números aleatórios é um processo importante na programação, pois permite que os desenvolvedores criem programas que requerem valores imprevisíveis. Isso pode ser útil em jogos, sorteios, criptografia e outras áreas da tecnologia.

## Como fazer:

```
# Gere um número inteiro aleatório entre 1 e 100
fish -c "echo (random 1 100)"

# Gere um número decimal aleatório entre 0 e 1
fish -c "echo (math random)"

# Gere uma string aleatória de 10 caracteres
fish -c "echo (pwgen -1 10)"
```

O primeiro comando usa a função `random` para gerar um número inteiro aleatório entre 1 e 100. O segundo comando usa a função `math random` para obter um número decimal aleatório entre 0 e 1. Por fim, o terceiro comando utiliza o utilitário `pwgen` para gerar uma string aleatória de 10 caracteres.

## Profundidade:

Gerar números aleatórios é uma técnica antiga na programação. Antes do advento dos computadores, dados como cartas de baralho e dados eram usados para criar resultados aleatórios. Hoje em dia, existem outras opções além da função `random` do Fish Shell, como o módulo `random` da linguagem Python e o utilitário `shuf` do UNIX. Essas alternativas oferecem funções e recursos adicionais para gerar números aleatórios em diferentes formatos e intervalos.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/cmds/random.html)
- [Módulo `random` do Python](https://docs.python.org/3/library/random.html)
- [Utilitário `shuf` do UNIX](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)