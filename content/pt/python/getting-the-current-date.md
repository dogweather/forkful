---
title:                "Obtendo a data atual"
html_title:           "Python: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que e Pq?
Pegar a data atual em um programa é uma função importante para lidar com informações cronológicas. Os programadores geralmente precisam dessa funcionalidade para rastrear o tempo de execução do código, realizar cálculos com datas ou simplesmente exibir a data para o usuário.

## Como fazer:
Para obter a data atual em Python, é necessário importar o módulo `datetime`, que possui a função `datetime.now()`, que retorna um objeto contendo a data e hora atuais. Em seguida, use a função `strftime()` para formatar a data da forma desejada.

```Python
import datetime

# Obtendo data atual
data_atual = datetime.now()

# Formatando a data
formato = data_atual.strftime("%d/%m/%Y")

# Imprimindo a data formatada
print(formato)
```

**Saída:**

> 21/07/2021

## Mergulho profundo:
A obtenção da data atual é um recurso útil em muitos cenários de programação. Antes do módulo `datetime` ser introduzido em Python, a obtenção da data atual era feita utilizando a função `time()`, que retornava apenas o tempo em segundos desde a meia-noite de 1º de janeiro de 1970. Além disso, existem outras formas de obter a data e hora atuais, como o uso de APIs externas.

## Veja também:
- Documentação do módulo `datetime` em Python: https://docs.python.org/3/library/datetime.html
- Python Anywhere - Como obter a data e hora atuais: https://help.pythonanywhere.com/pages/GettingTheCurrentDateInYourProgram/