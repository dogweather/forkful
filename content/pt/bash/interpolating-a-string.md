---
title:                "Interpolando uma string"
date:                  2024-01-20T17:50:36.504584-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Interpolar uma string é o ato de inserir variáveis ou expressões dentro de uma cadeia de texto, de modo a criar um novo valor. Programadores fazem isso para construir strings dinamicamente e facilitar a manipulação e apresentação de dados.

## Como Fazer:

```Bash
# Interpolação simples com variáveis.
nome="Mundo"
echo "Olá, $nome!"

# Sample output: Olá, Mundo!

# Utilizando chaves para delimitar o nome da variável.
preco=20
echo "O custo é ${preco} reais."

# Sample output: O custo é 20 reais.

# Executando um comando dentro de uma string.
echo "Estamos no diretório $(pwd)."

# Sample output: Estamos no diretório /caminho/para/diretorio.

# Interpolando strings com aritmética.
a=5
b=10
echo "Cinco mais dez é $((a + b))."

# Sample output: Cinco mais dez é 15.
```

## Mergulho Profundo:

Interpolar strings em Bash é uma prática que remonta aos primeiros dias do shell Unix. Usando essa técnica, torna-se mais fácil automatizar tarefas e personalizar mensagens sem a necessidade de concatenar strings e variáveis manualmente, o que economiza tempo e aumenta a legibilidade.

Existem alternativas à interpolação direta em Bash, como a concatenação explícita ou o uso do comando `printf`, que oferece maior controle sobre a formatação.

Quanto à implementação, quando o Bash encontra um `$` dentro de aspas duplas, ele sabe que precisa substituir o que segue pelo valor correspondente. Chaves são úteis para delimitar a variável, especialmente quando há texto adjacente que pode ser confundido como parte do nome da variável.

## Veja Também:

- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide: Variables and Parameters](http://www.tldp.org/LDP/abs/html/parameter-substitution.html)
- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
