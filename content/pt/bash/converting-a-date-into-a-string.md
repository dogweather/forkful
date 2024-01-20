---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertendo uma data em uma string em Bash

## O que e Por quê?

Converter uma data em uma string significa transformar um objeto de data num formato textual legível. Os programadores fazem isso para tornar as datas mais manipuláveis e fáceis de visualizar.

## Como fazer:

Vou mostrar como fazer essa conversão no bash com o comando `date`.

```Bash
$ data_atual=$(date +"%Y-%m-%d %H:%M:%S")
$ echo $data_atual 
```

Ao executar este script, terá o seguinte resultado:

```Bash
2022-03-22 14:37:33
```

Esse script captura a data atual no formato AAAA-MM-DD HH:MM:SS (ano-mês-dia hora:minuto:segundo).

## Aprofundando

Historicamente, a conversão entre datas e strings é um problema comum em programação. Em ambientes Unix-like, como Linux ou MacOS, o utilitário `date` tem sido a ferramenta padrão para essas tarefas.

Outras linguagens têm funções ou métodos embutidos para converter datas, como o `toString()` em JavaScript e o `.strftime()` em Python.

Entenda que a eficácia da função de conversão de datas em Bash depende do sistema operacional em que você está trabalhando. Em sistemas Windows, a abordagem será diferente, em que cygwin ou softwares similares são necessários para a simulação da terminal shell.

## Veja mais:

1. Manual do comando Date: [Link Aqui](https://man7.org/linux/man-pages/man1/date.1.html)
3. Guia de formatação da data do GNU: [Link Aqui](http://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)