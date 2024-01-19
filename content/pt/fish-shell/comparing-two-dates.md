---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas é um meio de determinar a diferença entre essas datas em termos de dias, meses, anos, etc. Programadores costumam fazer isso para lidar com prazos, eventos agendados ou para determinar a duração de certos processos.

## Como Fazer:

No Fish Shell, você pode usar o comando date incorporado com o operador de teste para comparar. Aqui estão os passos:

Primeiro, converta as datas para um formato comparável, como o formato Unix de marca de tempo (segundos desde 1 de janeiro de 1970).

```Fish Shell
set date1 (date -d "2020-01-01" +%s)
set date2 (date -d "2020-02-01" +%s)
```

Então, use o operador de teste para comparar as datas:

```Fish Shell
if test $date1 -gt $date2
    echo "date1 é posterior"
else if test $date1 -eq $date2
    echo "As datas são iguais"
else
    echo "date2 é posterior"
end
```

A saída seria "date2 é posterior".

## Aprofundamento:

Historicamente, a comparação de datas tem sido um pilar fundamental do gerenciamento de tempo em programação. Nas versões anteriores do Fish Shell, essa tarefa não era tão simples, precisando de contorções com strftime e outros comandos awk. 

Quanto às alternativas, em outros shells como Bash ou Zsh, você pode usar o comando 'date' de forma semelhante para comparar duas datas. Em linguagens de alto nível como Python ou JavaScript, existem bibliotecas ricas como datetime e moment.js.

Um detalhe de implementação importante no Fish é que as datas são convertidas em formato Unix (segundos desde 1970) usando date -d. Este comportamento pode variar dependendo do sistema operacional e da versão do comando date. Portanto, é sempre recomendado testar a lógica de comparação de datas no ambiente alvo.

## Veja Também:

1. Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
2. Guia de Manipulação de Data e Hora no Unix: https://www.cyberciti.biz/faq/unix-linux-bsd-appleosx-bash-get-time/
3. Comparação de datas no Bash: https://www.shellhacks.com/bash-date-difference/
4. Bibliotecas de Manipulação de Data e Hora: 
   - Python: https://docs.python.org/3/library/datetime.html
   - JavaScript: https://momentjs.com/