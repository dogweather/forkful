---
title:                "Bash: Comparando duas datas"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em programação Bash?

A comparação de datas é uma tarefa comum na programação, pois permite que você verifique se uma data é anterior, posterior ou igual a outra. Isso pode ser útil em inúmeras situações, como verificar a validade de um cartão de crédito, a data de validade de um produto ou até mesmo a idade de uma pessoa. Além disso, a comparação de datas é essencial em scripts ou programas que envolvem tarefas agendadas ou com prazos específicos.

## Como comparar duas datas em Bash

Para comparar duas datas em Bash, é necessário primeiro entender como as datas são representadas em formato texto. Em Bash, as datas são representadas em formato de data ISO (YYYY-MM-DD), com o traço separando o ano, mês e dia. 

Abaixo está um exemplo simples de um script Bash que compara duas datas inseridas pelo usuário:

```
#!/bin/bash

# Solicita ao usuário que insira a primeira data
echo "Insira a primeira data (formato YYYY-MM-DD):"
read data1

# Solicita ao usuário que insira a segunda data
echo "Insira a segunda data (formato YYYY-MM-DD):"
read data2

# Compara as duas datas usando o operador de maior que (>)
if [ "$data1" > "$data2" ]; then
    echo "$data1 é maior que $data2"
# Compara as duas datas usando o operador de igualdade (=)
elif [ "$data1" = "$data2" ]; then
    echo "$data1 é igual a $data2"
else
    echo "$data1 é menor que $data2"
fi
```

Supondo que o usuário insira a data "2020-05-15" como data1 e "2020-05-12" como data2, o output seria:

```
2020-05-15 é igual a 2020-05-12
```

## Aprofundando na comparação de datas em Bash

Além dos operadores de maior que (>) e igualdade (=), Bash também possui operadores de menor que (<) e maior ou igual que (>=), que podem ser usados para comparar datas em um formato de data ISO. 

Outra forma de representar datas em Bash é usando o timestamp, que é o número de segundos desde 1 de janeiro de 1970, 00:00:00 UTC. Para obter o timestamp de uma data específica no formato de data ISO, você pode usar o comando `date` com a opção `-d`, seguido da data desejada.

Existem também ferramentas úteis em linha de comando, como o `dateutils`, que podem facilitar a comparação de datas em Bash. Esta biblioteca permite adicionar, subtrair e comparar datas, além de fornecer funções específicas para dias da semana e meses.

## Veja também

- Documentação do Bash: https://www.gnu.org/software/bash/manual/bash.html
- Comandos básicos do Bash: https://www.hostinger.com.br/tutoriais/comandos-basicos-linux
- Documentação do dateutils: https://manpages.debian.org/testing/dateutils/dateutils.1.en.html