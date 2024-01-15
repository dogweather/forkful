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

## Por que

Você já se deparou com uma situação em que precisa capitalizar uma determinada string para que ela se encaixe no formato desejado? Ou talvez queira tornar seu código mais legível, deixando todas as palavras começando com letra maiúscula? Nesse caso, saber como capitalizar uma string pode ser uma habilidade muito útil em suas tarefas de programação.

## Como fazer

Para capitalizar uma string em Bash, temos algumas opções que podem ser utilizadas de acordo com a situação em que estamos trabalhando. Vamos dar uma olhada em algumas delas:

```
# Utilizando o comando "tr"
string="texto em caixa baixa"
capitalized_string=$(echo $string | tr '[:lower:]' '[:upper:]')
echo $capitalized_string
# Saída: TEXTO EM CAIXA BAIXA

# Utilizando o comando "sed"
string="texto em caixa baixa"
capitalized_string=$(echo $string | sed 's/\b\(.\)/\u\1/g')
echo $capitalized_string
# Saída: Texto Em Caixa Baixa
```

## Explorando mais a fundo

As duas opções apresentadas acima utilizam comandos externos do sistema operacional para realizar a capitalização da string. Entretanto, se desejarmos realizar essa tarefa diretamente no código Bash, podemos utilizar o recurso de substituição de padrões. Veja o exemplo abaixo:

```
string="texto em caixa baixa"
capitalized_string="${string^}"
echo $capitalized_string
# Saída: Texto em caixa baixa

capitalized_string="${string^^}"
echo $capitalized_string
# Saída: TEXTO EM CAIXA BAIXA
```

Com esse recurso, podemos escolher se queremos apenas a primeira letra de cada palavra em maiúsculo ou se queremos todas as letras em maiúsculo.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Guia de comandos do Linux](https://www.linuxcommand.org/index.php)
- [Tutorial de Bash para iniciantes](https://linuxize.com/post/bash-scripting-tutorial/)