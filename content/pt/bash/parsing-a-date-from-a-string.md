---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:35:05.924768-07:00
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Parsear uma data de uma string no Bash significa extrair e converter essa informação num formato que podemos manipular. Programadores fazem isso pra automatizar e tratar dados de calendário, como logs e timestamps.

## Como fazer:

```Bash
#!/bin/bash
data_string="2023-04-05 14:20:00"
data_formatada=$(date -d "$data_string" '+%d/%m/%Y %H:%M:%S')

echo "Data formatada: $data_formatada"
```

Output esperado:

```
Data formatada: 05/04/2023 14:20:00
```

Se você precisa só do dia, mês ou ano, é só mudar o '+%d/%m/%Y %H:%M:%S' para o que precisa, tipo '+%Y' pro ano:

```Bash
ano=$(date -d "$data_string" '+%Y')
echo "Ano: $ano"
```

Output:

```
Ano: 2023
```

## Aprofundamento:

Historicamente, a manipulação de datas no Unix e seus derivados sempre envolveu um bocado de malabarismo com comandos como `date`. Hoje em dia, no entanto, o Bash conta com `date` e outras ferramentas para facilitar esse processo.

Alternativas incluem usar AWK, Perl ou Python scripts chamados dentro do próprio Bash, cada um com seus próprios métodos de parsear datas. Escolher entre um método ou outro depende da complexidade da tarefa e da familiaridade com a linguagem.

Detalhes da implementação: `date -d` é uma opção do comando `date` que indica a data e hora a ser convertida. O formato de saída é especificado após o `+` e permite que você molde a data/hora no formato que precisar. Cada `%` seguido de um caractere especifica uma parte da data: `%Y` para ano, `%m` para mês, e daí em diante.

## Veja Também:

- [GNU Coreutils - Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) para mais informações sobre o comando `date`.
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/) para um guia mais abrangente de script em Bash.
- Stack Overflow tem várias perguntas e respostas sobre [parseamento de datas no Bash](https://stackoverflow.com/questions/tagged/date+bash).
