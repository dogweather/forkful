---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pegando a Data Atual no Fish Shell

## O que é e por quê?
Pegar a data atual significa obter a data e hora correntes do sistema. Programadores freqüentemente fazem isso para rastrear eventos, gerar logs de atividades e programar tarefas.

## Como fazer:
No Fish Shell, você pode pegar a data atual usando o comando `date`. Aqui está um rápido exemplo:

```fish
~> date
Tue May 4 14:02:49 BRST 2022
````

Este comando retorna a data e hora correntes no formato padrão.

Se quiser alterar o formato, você pode usar `+` seguido pela especificação de formato. Por exemplo, para obter apenas o dia da semana:

```fish
~> date "+%A"
Tuesday
```

## Mergulho Profundo
Com relação ao contexto histórico, o comando `date` tem raízes no Unix, sendo herdado de sistemas anteriores a este.

Existem algumas alternativas para pegar a data atual no Fish Shell, você poderia escrever um script em Python ou Perl, mas `date` é a maneira mais direta e menos verbosa.

Internamente, o comando `date` no Fish Shell, e na maioria dos outros shells Unix, chama a função de sistema `time()`, que retorna a hora atual, expressa em segundos desde 01/01/1970, e então formata esta informação conforme requerido.

## Veja também:
Se você está começando a programar no Fish Shell, há muitos recursos disponíveis para você. Aqui estão apenas alguns para você começar:

- Documentação Oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Guia de Referência GNU para 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Guia de Introdução ao Fish Shell: https://fishshell.com/docs/current/tutorial.html

Espero que você encontrou esta explicação útil e direto ao ponto. Agora é sua vez de experimentar!