---
title:    "Bash: Comparando duas datas"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas em Programação Bash?

A comparação de datas é uma tarefa comum em programação, especialmente em Bash, onde as datas são frequentemente usadas para controle de fluxo e tomada de decisões. Ao comparar duas datas, podemos determinar qual delas é mais recente ou se elas são iguais, dando-nos informações importantes para executar nosso código corretamente.

## Como Comparar Duas Datas em Bash

Comparar duas datas em Bash é feito usando o comando `test`, também conhecido como `[`. Nós usamos a opção `-nt` (mais novo que) para determinar se a primeira data é mais recente que a segunda data, ou seja:

```
#!/bin/bash

data_1="2021-10-15"
data_2="2021-10-10"

if [ $data_1 -nt $data_2 ]; then
    echo "Data 1 é mais recente que Data 2."
fi
```

Se quisermos comparar se duas datas são iguais, usamos a opção `-eq` (igual a):

```
#!/bin/bash

data_1="2021-10-15"
data_2="2021-10-15"

if [ $data_1 -eq $data_2 ]; then
    echo "As datas são iguais."
fi
```

Podemos até mesmo comparar a data atual com uma data específica, usando o comando `date` para gerar a data atual no formato que desejamos:

```
#!/bin/bash

data_atual=$(date +"%Y-%m-%d")
data_referencia="2021-10-01"

if [ $data_atual -gt $data_referencia ]; then
    echo "Já passou do dia 1 de outubro."
fi
```

## Mergulho Profundo na Comparação de Datas

Ao comparar datas, devemos ter cuidado para que elas estejam no formato correto. No nosso exemplo, usamos o formato "aaaa-mm-dd", mas também poderíamos usar "dd/mm/aaaa" ou "dd-mm-aaaa". Além disso, é importante lembrar que a ordem em que as datas são escritas no código é importante, uma vez que estamos comparando strings de texto.

Uma opção para comparar datas mais complexas, como horários, é o uso do comando `date` juntamente com o formato "epoch". Este formato representa o número de segundos passados desde 1º de janeiro de 1970. Podemos converter datas para esse formato usando `date -d` e, em seguida, comparar os valores resultantes.

## Veja Também

- [Documentação do comando `test` em Bash] (https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#Bourne-Shell-Builtins)
- [Documentação do comando `date` em Bash] (https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Date-Manipulation.html#Bourne-Shell-Date-Manipulation)

Esperamos que este artigo tenha sido útil para entender como comparar duas datas em Bash. Com a comparação de datas em seu repertório de habilidades de programação, você poderá escrever códigos mais eficazes e precisos.