---
title:                "Convertendo uma data em uma string."
html_title:           "Fish Shell: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma data para uma string é um processo em que uma data em um formato específico é transformada em uma sequência de caracteres representando essa data. Isso pode ser útil para exibir datas em um formato mais legível ou para uso em funções de comparação de datas.

Programadores realizam essa conversão para facilitar a manipulação e visualização de datas em seus códigos. Eles também podem precisar converter datas em strings para atender a requisitos específicos de sistemas ou APIs.

## Como fazer:

Para converter uma data para uma string em Fish Shell, podemos usar o comando `date` junto com a opção `-s` para especificar o formato desejado. Por exemplo, se quisermos exibir a data atual no formato DD/MM/AAAA, podemos usar o seguinte comando:

```Fish Shell
date -s "%d/%m/%Y"
```

Isso fará com que a saída seja exibida no seguinte formato: 23/12/2021. Podemos alterar o formato conforme necessário, consultando a documentação do comando `date`.

É importante notar que o formato utilizado pode variar de acordo com o sistema operacional. Por exemplo, no MacOS podemos usar `date -I` para obter a data atual no formato AAAA-MM-DD.

## Mergulho profundo:

O processo de converter uma data em uma string tem suas raízes na programação de computadores desde o início. Com o avanço da tecnologia, surgiram várias maneiras de realizar essa conversão, como a biblioteca `strftime` em C ou a função `ToStr` em Pascal.

Além do `date`, também existem outras opções disponíveis em Fish Shell para trabalhar com datas, como o comando `strftime` que também permite a formatação de datas e o uso de operadores lógicos para realizar comparações de datas.

A implementação do comando `date` pode variar de sistema para sistema, mas geralmente utiliza chamadas de sistema do sistema operacional para obter a data atual e, em seguida, a formatação é aplicada usando a biblioteca `strftime`.

## Para mais informações:

- Documentação do comando `date` em Fish Shell: https://fishshell.com/docs/current/cmds/date.html
- Comparando datas em Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_dates
- Biblioteca `strftime` em C: https://www.tutorialspoint.com/c_standard_library/time_h.htm