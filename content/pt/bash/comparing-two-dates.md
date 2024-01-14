---
title:                "Bash: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Bash?

Comparar datas em Bash é uma habilidade útil para qualquer programador que precisa lidar com dados relacionados a tempo. Essa comparação pode ser útil para verificar a validade de dados, encontrar diferenças entre datas ou simplesmente organizar informações por ordem cronológica.

## Como fazer a comparação

Para comparar datas em Bash, podemos usar o comando `date` em conjunto com operadores de comparação, como `=` (igual), `!=` (diferente), `<` (menor que) e `>` (maior que). Vamos supor que queremos verificar se a data atual é depois do dia 1 de Janeiro de 2021. Podemos usar o seguinte código:

```Bash
if [ $(date +%s) -gt $(date -d "2021-01-01" +%s) ]; then
  echo "A data atual é depois de 01/01/2021."
fi
```

Nesse código, usamos o comando `date` para obter a data atual e a data especificada. O parâmetro `%s` converte a data para um formato de segundos, tornando a comparação mais fácil. Em seguida, usamos o operador `-gt` para verificar se a data atual é maior que a data especificada. Se a condição for verdadeira, a mensagem será exibida.

Podemos fazer comparações mais complexas, como verificar se uma data é antes ou depois de um intervalo específico. Por exemplo:

```Bash
if [ $(date +%s) -lt $(date -d "2021-01-01" +%s) ] && \
   [ $(date +%s) -gt $(date -d "2020-01-01" +%s) ]; then
  echo "A data atual está entre 01/01/2020 e 01/01/2021."
fi
```

Nesse caso, usamos os operadores lógicos `&&` (e) e `||` (ou) para combinar as condições. Dessa forma, podemos verificar se a data atual está dentro de um determinado intervalo.

## Deep Dive

Uma coisa importante a se considerar ao comparar datas em Bash é o formato da data. O comando `date` pode receber diferentes parâmetros para especificar o formato desejado. Por exemplo, `%s` retorna a data em segundos, `%F` retorna a data no formato "AAAA-MM-DD" e `%Y` retorna o ano de forma numérica de 4 dígitos.

Além disso, podemos converter uma data em um formato específico usando o comando `date` novamente. Por exemplo:

```Bash
date -d "2021-01-01" +%s
```

Isso converterá a data "2021-01-01" para segundos. O mesmo pode ser feito com outras datas e formatos, permitindo uma maior flexibilidade na comparação de datas.

## Veja também

Aqui estão alguns links úteis sobre como comparar datas em Bash:

- [Documentação do comando date](https://www.gnu.org/software/coreutils/date)
- [Guia de comparação de strings em Bash](https://linuxhint.com/bash_string_comparison/)
- [Exemplos práticos de comparação de datas em Bash](https://linuxhint.com/bash_string_comparison/)