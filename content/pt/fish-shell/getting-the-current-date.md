---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:14:09.625840-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Pegar a data atual em programação é simplesmente consultar o sistema para saber que dia é hoje. Programadores fazem isso para registrar eventos, controlar prazos ou exibir informações atualizadas para usuários.

## Como Fazer:
Aqui estão alguns exemplos de como você pode obter a data atual no Fish Shell:

```Fish Shell
# Exemplo para pegar a data completa:
set data_atual (date)
echo $data_atual
# Output: Ter 03 Mar 2023 15:45:00 BRT

# Para obter apenas o ano:
set ano_atual (date +"%Y")
echo $ano_atual
# Output: 2023

# Para o mês e o dia:
set mes_dia_atual (date +"%m-%d")
echo $mes_dia_atual
# Output: 03-03
```

## Mergulho Profundo
A função `date` é utilizada em muitos sistemas operacionais baseados em Unix e foi herdada pelo Fish Shell. Antigamente, cada sistema tinha sua própria maneira de lidar com datas, mas com o tempo padronizou-se com a chamada `date`. Existem alternativas, como comandos em Perl ou Python, mas `date` é a forma nativa mais direta e performática. O Fish Shell executa isto de maneira interna; você não precisa de preocupar com as diferenças entre sistemas operacionais.

## Veja Também
- Documentação Oficial do Fish Shell sobre comandos embutidos: [https://fishshell.com/docs/current/commands.html#date](https://fishshell.com/docs/current/commands.html#date)
- Mais sobre scripts no Fish Shell: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)