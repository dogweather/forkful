---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pegando a data atual no Bash: Um guia conciso

## O que é & Por quê?

Obter a data atual no Bash refere-se a exibir a data e a hora presentes no sistema via linha de comando. Programadores fazem isso para rastrear o tempo, arquivar registros ou marcar operações.

## Como fazer:

Você pode obter a data atual em Bash com este comando:

```bash
date
``` 

Isso vai exibir algo como:

```bash
Qua Set 22 14:27:05 UTC 2021
```

Para formatar a saída da data, você pode usar '+' seguido por um formato específico, por exemplo:

```bash
date +"%d/%m/%Y"
```

Tal saída será:

```bash
22/09/2021
```

## Deep Dive

A função de data do Bash existe desde as primeiras versões da linguagem. Ela usa as bibliotecas do sistema para extrair e formatar a data atual com base no fuso horário definido no sistema.

As alternativas para obter a data incluem o uso de outras linguagens de programação ou ferramentas, como Perl, Python ou o comando 'awk'. No entanto, 'date' é uma opção simples e disponível em quase todos os sistemas Unix-like.

Detalhes importantes da implementação do 'date' incluem que ele depende das definições de fuso horário do sistema e das localizações. Mude essas configurações e a saída do 'date' mudará também.

## Veja também

1. Manual do Bash 'date': https://man7.org/linux/man-pages/man1/date.1.html
2. Formatação de data no Bash: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
3. Tutorial completo de Bash: https://www.learnshell.org/
4. Alternativas de comando para 'date': https://alternativeto.net/software/date-command/
5. Detalhes da 'date' no stack overflow: https://stackoverflow.com/questions/tagged/date