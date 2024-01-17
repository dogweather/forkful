---
title:                "Obtendo a data atual"
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# O que e por que?

Obter a data atual é uma tarefa comum para muitos programadores em Bash. Isso permite que eles obtenham informações precisas sobre o momento em que um script é executado e a usem para criar lógicas ou gerar arquivos com nomes únicos. Além disso, a data atual é uma informação importante para fins de registro e identificação de tendências.

# Como fazer:

Existem várias maneiras de obter a data atual em Bash. Aqui estão dois exemplos:

```
# A saída será no formato "MM/DD/AAAA"
Bash ... date +"%m/%d/%Y"

# A saída será no formato "Dia da semana, DD de Mês de AAAA"
Bash ... date +"%A, %d de %B de %Y"
```

Nota: os três "%A, %d, %B" são especificadores de formato de data, que podem ser encontrados na documentação do "date". Você pode experimentar diferentes combinações para personalizar o formato da data.

# Aprofundamento:

Historicamente, a maioria dos sistemas Unix e Linux possuem o comando "date" incorporado que permite obter a data atual. No entanto, existem outras ferramentas ou dependências que também podem ser usadas, como o "time" ou o "localtime". Além disso, há a opção de usar a linguagem de programação "Python" com o módulo "datetime" ou o utilitário "awk".

# Veja também:

- A documentação oficial do "date" para mais informações sobre especificadores de formato de data - https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- O artigo "Formatar a data e a hora em Bash" para mais exemplos e dicas - https://linuxize.com/post/bash-formatting-date-time/
- O guia "Como usar o utilitário date em Linux" para mais informações e alternativas - https://www.linux.com/topic/desktop/date-command-linux-tips-and-tricks/