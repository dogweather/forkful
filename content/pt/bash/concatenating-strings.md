---
title:                "Bash: Unir cadeias de caracteres"
simple_title:         "Unir cadeias de caracteres"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma técnica fundamental em programação, especialmente na linguagem Bash. Isso permite que você junte várias strings em uma única, criando uma nova string personalizada de acordo com a sua necessidade.

## Como Fazer

Existem algumas maneiras de concatenar strings em Bash. Aqui estão três métodos comuns usando aspas duplas, aspas simples e sem aspas:

```Bash
# Usando aspas duplas
nome="Maria"
sobrenome="Silva"
nome_completo="$nome $sobrenome"
echo "Seu nome completo é $nome_completo"

# Saída: Seu nome completo é Maria Silva

# Usando aspas simples
nome="João"
sobrenome="Sousa"
nome_completo='$nome $sobrenome'
echo "Seu nome completo é $nome_completo"

# Saída: Seu nome completo é $nome $sobrenome

# Sem usar aspas
nome="Pedro"
sobrenome="Almeida"
nome_completo=$nome$sobrenome
echo "Seu nome completo é $nome_completo"

# Saída: Seu nome completo é PedroAlmeida
```

## Deep Dive

Concatenar strings é uma operação simples em Bash, mas você pode enfrentar alguns desafios ao lidar com caracteres especiais, como espaços ou aspas. Nesses casos, é necessário usar aspas para garantir que as strings sejam interpretadas corretamente.

Além disso, é importante lembrar que strings são imutáveis em Bash, o que significa que não é possível alterar uma string já definida. Portanto, sempre que precisar concatenar strings, você deve criar uma nova variável para armazenar o resultado.

## Veja Também

- [Documentação do Bash sobre Strings](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Mais exemplos de concatenação de strings em Bash](https://stackoverflow.com/questions/4181703/how-to-concatenate-string-variables-in-bash)