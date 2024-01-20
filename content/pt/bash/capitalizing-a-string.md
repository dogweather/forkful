---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizando Strings em Bash

## O que e por que?

Capitalizar uma string é transformar todas as suas letras iniciais em maiúsculas. Os programadores fazem isso para melhorar a legibilidade e padronização dos dados de texto.

## Como fazer:

```Bash
# Definindo a string
nome="meu nome é bash"

# Transformando em maiúsculas
nome_capitalizado="${nome^}"

echo $nome_capitalizado
```

A saída será:

```Bash
Meu nome é bash
```

Para capitalizar toda a string, usamos a seguinte sintaxe:

```Bash
nome="meu nome é bash"

# Transformando todos em maiúsculas
nome_todo_capitalizado="${nome^^}"

echo $nome_todo_capitalizado
```

Saída:

```Bash
MEU NOME É BASH
```

## Contextualização:

Historicamente, a capacidade de capitalizar strings em Bash só foi introduzida na versão 4.0, lançada em 2009. Antes disso, as pessoas tinham que usar truques, como passar a string por um comando de substituição com `tr`.

```Bash
echo "meu nome é bash" | tr '[:lower:]' '[:upper:]'
```

Quanto aos detalhes de implementação, o Bash usa as regras de localidade (LC_CTYPE) do sistema para determinar quais caracteres devem ser convertidos para maiúsculas.

## Veja mais:

- Manual do Bash (https://www.gnu.org/software/bash/manual/bash.html)
- Guia de programação em Bash (https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- Wiki do Bash (https://wiki.bash-hackers.org/)