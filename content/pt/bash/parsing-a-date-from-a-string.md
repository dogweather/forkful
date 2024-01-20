---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analisando Datas a Partir de Strings em Bash

## O Que & Por Que?
Analisar datas a partir de strings envolve a conversão de uma string que representa uma data ou hora em uma forma mais utilizável para o programador. Fazemos isso para manipular ou comparar datas de uma maneira que seja significativa para o nosso programa.

## Como Fazer:
Vamos utilizar o comando `date` do Bash. Ele interpreta uma string de data e tempo arbitrária. Veja este exemplo:

```Bash
data_string="2021-09-23 17:20:15"
data=$(date -d "$data_string" +'%Y-%m-%d %H:%M:%S')

echo $data
```

A saída será:

```Bash
2021-09-23 17:20:15
```

## Mergulhando Fundo
Análise de datas a partir de strings é uma prática comum em programação que data de tempos anteriores aos computadores modernos. No mundo do Bash, a alternativa ao comando `date` é usar o GNU `date` ou manipular strings manualmente, o que é mais complexo e sujeito a erros.

Embora este artigo mostre um exemplo simples, implementações reais podem exigir que se considerem detalhes mais complicados, como fusos horários, configurações regionais e formatos de data incomuns.

## Veja Também
Para mais informações e exemplos mais profundos sobre a análise de datas a partir de strings em Bash, você pode verificar:
1. [Data e Hora no Bash](https://linuxize.com/post/how-to-compare-dates-in-bash/)
2. [GNU Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
3. [Tutorial detalhado sobre string de data e hora](https://ryanstutorials.net/bash-scripting-tutorial/bash-time.php)