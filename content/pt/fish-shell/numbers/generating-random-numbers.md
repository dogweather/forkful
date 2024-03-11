---
date: 2024-01-27 20:33:37.400261-07:00
description: "Gerar n\xFAmeros aleat\xF3rios \xE9 uma tarefa fundamental em programa\xE7\
  \xE3o, usada para tudo desde amostragem de dados at\xE9 o desenvolvimento de jogos.\
  \ No Fish Shell,\u2026"
lastmod: '2024-03-11T00:14:20.738114-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios \xE9 uma tarefa fundamental em programa\xE7\
  \xE3o, usada para tudo desde amostragem de dados at\xE9 o desenvolvimento de jogos.\
  \ No Fish Shell,\u2026"
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
---

{{< edit_this_page >}}

## O Que & Por Que?

Gerar números aleatórios é uma tarefa fundamental em programação, usada para tudo desde amostragem de dados até o desenvolvimento de jogos. No Fish Shell, fazer uso de ferramentas do sistema e funções incorporadas para este propósito permite que programadores incorporem aleatoriedade e variabilidade em scripts e aplicações de forma eficaz.

## Como fazer:

Gerar um número aleatório no Fish pode ser direto, usando a combinação de utilitários do sistema e capacidades do shell. Abaixo estão alguns exemplos demonstrando como gerar números aleatórios dentro de intervalos especificados.

**Gerar um número aleatório entre 0 e 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Saída Exemplar:**
```fish
42
```

**Gerar um número aleatório entre dois números quaisquer, digamos 50 e 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Saída Exemplar:**
```fish
103
```

**Usando random para embaralhar uma lista:**

Você também pode querer embaralhar elementos aleatoriamente em uma lista. Veja como você pode fazer isso:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Saída Exemplar:**
```fish
C
A
E
D
B
```

Por favor, note que a saída variará todas as vezes que você executar esses comandos devido à natureza da aleatoriedade.

## Estudo Aprofundado

A função `random` do Fish Shell oferece uma interface fácil de usar para a geração de números pseudoaleatórios. Internamente, ela envolve utilitários de geração de números aleatórios a nível de sistema, oferecendo uma maneira portátil de introduzir aleatoriedade em seus scripts. No entanto, é essencial lembrar que a aleatoriedade fornecida por `random` é suficiente para a maioria das tarefas de script, mas pode não atender aos requisitos de segurança criptográfica para aplicações que precisam de um grau de imprevisibilidade mais alto.

Para contextos de segurança de alta importância, considere o uso de ferramentas dedicadas ou bibliotecas de programação projetadas para fins criptográficos, que fornecem garantias de aleatoriedade mais fortes. No entanto, para scripts gerais e aplicações onde os mais altos padrões de segurança para aleatoriedade não são um requisito, a função `random` do Fish Shell oferece uma solução conveniente e eficaz.
