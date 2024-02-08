---
title:                "Analisando uma data a partir de uma string"
date:                  2024-02-03T19:13:42.362962-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Quê?

Analisar uma data de uma string em Bash envolve extrair e converter informações de data de dados textuais para um formato que o Bash pode manipular ou usar para processos posteriores. Isso é um requisito comum em scripts para tarefas como análise de arquivos de log, organização de arquivos com base em marcas de data ou relatórios automatizados, tornando-se uma habilidade essencial para programadores gerenciarem e utilizarem dados temporais de forma eficaz.

## Como fazer:

O próprio Bash é bastante limitado na capacidade direta de análise de datas, muitas vezes dependendo de ferramentas externas como `date` e `awk` para manipulações mais sofisticadas. Veja como você pode analisar um formato específico e, em seguida, usá-lo com o comando `date` para converter ou realizar operações.

**Exemplo 1:** Extrair uma string de data e convertê-la para outro formato.

Suponha que você tenha uma data no formato `aaaa-mm-dd` e deseje convertê-la para `dd-mm-aaaa`.

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**Saída Exemplo:**
```
01-04-2023
```

Isso usa o comando `date` com a opção `-d` para especificar a string de data de entrada, e `+%d-%m-%Y` para formatar a saída.

**Exemplo 2:** Usando `awk` para analisar uma data de uma linha de texto estruturado e convertê-la.

Supondo que você tenha uma linha de arquivo de log:

```
2023-04-01 12:00:00 Usuário conectou
```

Você pode extrair e converter a parte da data usando `awk` e `date`.

```bash
log_line="2023-04-01 12:00:00 Usuário conectou"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**Saída Exemplo:**
```
Sábado, Abril 01, 2023
```

Este exemplo usa `awk` para dividir a linha do arquivo de log e extrair a parte da data (`$1` representa o primeiro campo delimitado por espaço), e então `date` é usado para reformata-la.

### Usando ferramentas de terceiros

Para análises mais complexas ou ao lidar com uma ampla variedade de formatos de data, ferramentas de terceiros como `dateutils` podem ser muito úteis.

**Exemplo com `dateutils`:**

Supondo que você tenha uma string de data em um formato não padrão, por exemplo, `Abril 01, 2023`.

```bash
original_date="Abril 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**Saída Exemplo:**
```
2023-04-01
```

Este comando usa `dateconv` do `dateutils`, especificando o formato de entrada com `-i` e o formato de saída desejado com `-f`. `dateutils` suporta uma vasta gama de formatos de data e hora, tornando-o muito versátil para tarefas de análise de data em scripts Bash.
