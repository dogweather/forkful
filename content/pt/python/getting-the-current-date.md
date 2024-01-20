---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Obter a data atual é algo que programadores Python fazem para rastrear eventos e horários em tempo real. É especialmente útil para registro de logs, relatórios e programação orientada a eventos.

## Como Fazer:

No Python, você pode obter a data atual utilizando o módulo `datetime`. Veja o procedimento abaixo:

```python
import datetime

# Pegar a data atual
data_atual = datetime.date.today()
print(data_atual)
```

A saída seria algo como:

```python
2022-03-09  # Este é apenas um exemplo, a saída real seria a data atual.
```

## Mergulho Profundo

O módulo `datetime` pertence à biblioteca padrão do Python, então você não precisa instalar nada. É preferível usá-lo em vez de módulos externos, porque ele é mantido pelos desenvolvedores do Python e é altamente confiável.

Existem outras maneiras de obter a data atual, principalmente usando o módulo `time`, mas o `datetime` é geralmente preferido devido à sua flexibilidade e facilidade de uso. `datetime` também oferece funções para fazer cálculos de tempo, converter entre diferentes formatos de data e hora, e lidar com fuso horário.

No entanto, existem bibliotecas de terceiros como `Pendulum` e `Arrow` que podem oferecer alguns recursos extras, como suporte aprimorado para fusos horários e operações de data mais intuitivas, caso você precise deles.

## Veja Também:

Documentação oficial do Python sobre o módulo `datetime`: https://docs.python.org/3/library/datetime.html 

Para uma visão geral aprofundada e exemplos de `datetime`, 'Pendulum' e `Arrow`, tenha um olhar: 

- Artigo sobre 'Datetime' em Python: https://realpython.com/python-datetime/ 
- Documentação da biblioteca 'Pendulum': https://pendulum.eustace.io/docs/ 
- Documentação da biblioteca 'Arrow': https://arrow.readthedocs.io/en/latest/