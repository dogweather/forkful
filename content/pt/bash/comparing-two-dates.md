---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Comparar duas datas é verificar qual é a mais recente ou se são iguais. Programadores fazem isso para gerir eventos que ocorrem em tempos específicos, como backups agendados ou registrando quando um usuário fez login pela última vez.

## Como fazer: 

Vamos usar o comando `date` para pegar as datas e o comando `if` para fazer a comparação.

```Bash
# Obtemos a data atual
data_atual=$(date +%Y%m%d)

# Definimos uma data para comparação
data_comparacao=$(date -d"2022-12-01" +%Y%m%d)

#Fazendo a comparação
if [[ $data_atual -gt $data_comparacao ]] 
then
  echo "A data atual é maior que a data de comparação."
elif [[ $data_atual -eq $data_comparacao ]] 
then
  echo "As datas são iguais."
else
  echo "A data de comparação é maior que a data atual."
fi
```

Saída de amostra:

```Bash
A data atual é maior que a data de comparação.
```

## Deep Dive

Comparar datas é uma prática comum na programação desde os primeiros dias do UNIX, com a introdução do comando `date`. Existem alternativas para comparar datas em Bash, incluindo o uso de `date -d` ou `strtotime`, que oferecem mais flexibilidade em termos de formatos de data.

A implementação específica do comando `date` e da operação de comparação pode variar ligeiramente dependendo do sistema operacional e da versão do Bash. Os exemplos fornecidos presumem o uso de uma shell Bash recente (versão 4.x ou posterior) e podem não funcionar corretamente em versões mais antigas ou em shells derivadas do csh.

## Veja também

Para mais detalhes sobre a comparação de datas e as funções date em Bash, consulte os seguintes recursos:

- Manual do Bash: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Guia Avançado de Scripting Bash: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)