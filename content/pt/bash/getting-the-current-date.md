---
title:    "Bash: Obtendo a data atual"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante?

Obter a data atual é uma tarefa importante para qualquer programador de Bash. Isso permite que você automatize tarefas, como nomear arquivos com datas, criar logs diários ou verificar quando determinada ação foi executada.

Além disso, ter a data atualizada é essencial para manter a precisão e organização em seus projetos. Sempre que precisar de informações sobre quando um determinado arquivo foi criado ou modificado, ter a data atual pode ser muito útil.

## Como Obter a Data Atual em Bash

Obter a data atual em Bash é simples e possui diversas maneiras de ser feito. Vamos dar uma olhada em algumas delas:

```Bash
# Obtendo a data atual no formato padrão
data_atual=$(date)

# Obtendo a data atual no formato dd/mm/aaaa
data_formatada=$(date +%d/%m/%Y)

# Obtendo o dia e mês atual
dia=$(date +%d)
mes=$(date +%m)

echo "Hoje é dia $dia do mês $mes."
```

Ao executar esses comandos em seu terminal, você irá obter a data atual ou a data formatada no seu respectivo formato. Lembre-se que existem diversas opções de formatação, como %Y (ano com quatro dígitos), %y (ano com dois dígitos), %m (mês com dois dígitos), %d (dia com dois dígitos), entre outros. Você pode combiná-los da maneira que preferir para obter o formato desejado.

Outra maneira de obter a data atual é utilizando o comando `date +%A`, que irá retornar o dia da semana. E para exibir a hora atual, você pode utilizar `date +%H:%M:%S`.

## Deep Dive: Obtendo informações adicionais sobre a data atual

Agora que você já sabe como obter a data atual em Bash, vamos dar uma olhada em alguns comandos que podem te ajudar a extrair informações adicionais sobre dia, mês e ano.

- `date +%j` retorna o número do dia do ano (de 1 a 365 ou 366 em anos bissextos)
- `date +%U` retorna o número da semana do ano (de 0 a 53)
- `date +%w` retorna o dia da semana em formato numérico (de 0 a 6, onde 0 representa domingo)
- `date +%b` retorna o nome abreviado do mês atual (ex: jan, fev, mar)
- `date +%B` retorna o nome completo do mês atual (ex: janeiro, fevereiro, março)

Lembre-se sempre de utilizar o comando `man date` para visualizar outras opções de formatação e aprofundar seus conhecimentos sobre o comando `date` em Bash.

## Veja Também

- [Tutorial de Bash para iniciantes](https://dev.to/bricourse/bash-para-iniciantes-gm6)
- [Documentação oficial do comando `date`](https://man7.org/linux/man-pages/man1/date.1.html)