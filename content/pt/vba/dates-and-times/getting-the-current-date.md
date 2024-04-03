---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:43.270650-07:00
description: "No Visual Basic for Applications (VBA), recuperar a data atual \xE9\
  \ uma tarefa comum que permite aos programadores trabalhar dinamicamente com datas\
  \ em suas\u2026"
lastmod: '2024-03-13T22:44:46.423756-06:00'
model: gpt-4-0125-preview
summary: "No Visual Basic for Applications (VBA), recuperar a data atual \xE9 uma\
  \ tarefa comum que permite aos programadores trabalhar dinamicamente com datas em\
  \ suas macros ou aplica\xE7\xF5es."
title: Obtendo a data atual
weight: 29
---

## Como fazer:
Recuperar a data atual no VBA é direto, utilizando a função `Date`, enquanto a função `Now` fornece tanto a data quanto a hora atuais. Veja como você pode trabalhar com ambos:

```vb
Sub GetCurrentDate()
    ' Usando a função Date para obter a data atual
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Data Atual: "; currentDate
    
    ' Usando a função Now para obter a data e a hora atuais
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Data e Hora Atuais: "; currentDateTime
End Sub
```

Quando você executa essa macro, o método `Debug.Print` exibe a data atual e a data e hora atuais na Janela Imediata no editor do VBA. Por exemplo:

```
Data Atual: 12/04/2023
Data e Hora Atuais: 12/04/2023 15:45:22
```

Lembre-se de que o formato da data pode variar com base nas configurações do sistema do computador do usuário.

## Aprofundamento
As funções `Date` e `Now` encapsulam a complexidade de lidar com data e hora no Visual Basic for Applications, fornecendo uma abstração em nível de aplicação que torna o trabalho com datas simples e intuitivo. Historicamente, lidar com data e hora na programação tem sido repleto de desafios, incluindo o manejo de diferentes fusos horários, mudanças no horário de verão e diversos formatos de data.

No VBA, essas funções dependem da data e hora do sistema subjacente, o que significa que são influenciadas pela localidade e configurações do sistema do usuário. É uma espada de dois gumes que garante consistência com o ambiente do usuário, mas também exige um tratamento cuidadoso da localização e ajustes de fuso horário em aplicações globais.

Embora as funções de data e hora do VBA sejam perfeitamente adequadas para muitas aplicações, especialmente no escopo da automação do Office, elas podem não ter a precisão ou granularidade necessárias para aplicações mais complexas, como sistemas de negociação de alta frequência ou simulações científicas. Nesses casos, outros ambientes ou linguagens de programação, como Python ou C#, podem oferecer bibliotecas de manipulação de data e hora mais sofisticadas.

No entanto, para a grande maioria das tarefas envolvendo datas e horas no contexto do Excel, Word ou outras aplicações do Office, as funções `Date` e `Now` do VBA oferecem um equilíbrio de simplicidade, desempenho e facilidade de uso que é difícil de superar.
