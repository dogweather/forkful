---
title:                "Bash: Unindo strings"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em Bash?

A concatenação de strings é um conceito importante em linguagens de programação, incluindo o Bash. Ela é usada para unir duas ou mais strings em uma única sequência de caracteres. Isso pode ser útil em diferentes situações, como ao formar nomes de arquivos, criar saídas personalizadas ou construir comandos mais complexos.

## Como fazer a concatenação de strings em Bash

Para concatenar strings em Bash, usamos o operador `+` ou simplesmente colocamos as strings juntas dentro de aspas duplas. Vamos dar uma olhada em alguns exemplos para entender melhor.

Em primeiro lugar, podemos usar o operador `+` para concatenar duas ou mais strings. Por exemplo:

```Bash
primeiro_nome="João"
ultimo_nome="Silva"
nome_completo=$primeiro_nome+$ultimo_nome
echo $nome_completo
```

A saída será `JoãoSilva`, pois o operador `+` simplesmente une as strings sem adicionar nenhum espaço entre elas. Para adicionar um espaço, podemos usar a concatenação de strings com aspas duplas:

```Bash
saudacao="Olá"
nome="Maria"
saudacao_nome=$saudacao" "$nome
echo $saudacao_nome
```

A saída será `Olá Maria`, pois usamos aspas duplas para incluir um espaço entre as duas strings.

Podemos também concatenar variáveis com valores numéricos:

```Bash
numero1=10
numero2=5
soma="A soma dos números é: "$numero1+$numero2
echo $soma
```

A saída será `A soma dos números é: 15`, pois o operador `+` também pode ser usado para somar valores numéricos.

## Aprofundando na concatenação de strings em Bash

O operador `+` não é o único método para concatenar strings em Bash. Podemos também usar o comando `printf` para criar strings formatadas.

```Bash
primeiro_nome="Ana"
ultimo_nome="Santos"
printf "Seu nome completo é: %s %s\n" $primeiro_nome $ultimo_nome
```

A saída será `Seu nome completo é: Ana Santos`, pois usamos a formatação `%s` para indicar a posição das duas variáveis em nossa string.

Além disso, é importante lembrar que a ordem da concatenação afeta o resultado. Por exemplo:

```Bash
nome="Pedro"
sobrenome="Silva"
nome_completo=$sobrenome", "$nome
echo $nome_completo
```

A saída será `Silva, Pedro`, pois invertemos a ordem da concatenação colocando o sobrenome antes do nome.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial de Bash no DevMedia](https://www.devmedia.com.br/tutoriais/bash/)
- [Tutorial de Bash na Udemy](https://www.udemy.com/topic/bash-scripting/)