---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Python: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

O que é e por que calcular uma data no futuro ou no passado?

Calcular uma data no futuro ou no passado é um processo comum na programação, que envolve determinar uma data com base em uma data inicial e um número de dias a serem adicionados ou subtraídos. Isso pode ser útil em diversas situações, como criar lembretes de eventos futuros ou fazer cálculos de previsão.

Como fazer:

Para calcular uma data no futuro ou no passado em Python, podemos utilizar o método 'timedelta' da biblioteca 'datetime'. Veja abaixo um exemplo de código que calcula a data de 30 dias a partir da data atual:

```Python
from datetime import datetime, timedelta
data_atual = datetime.now()
data_futura = data_atual + timedelta(days=30)
print("Data Futura: " + str(data_futura))
```

O resultado será:

```Python
Data Futura: 2020-11-26 20:44:43.958298
```

Mergulho mais profundo:

Historicamente, o cálculo de datas no futuro ou no passado era feito manualmente pelos programadores, o que podia levar a erros e dificultava a manutenção do código. Hoje em dia, com a facilidade de uso e a precisão de bibliotecas como o 'datetime', essa tarefa se tornou muito mais simples e eficiente.

Além disso, existem também outras formas de realizar esse cálculo, como utilizando a biblioteca 'arrow' ou criando funções personalizadas. No entanto, o método 'timedelta' ainda é considerado o mais eficaz e recomendado.

Para implementar o cálculo de datas no futuro ou no passado em seus projetos, é importante conhecer bem os parâmetros do método 'timedelta', como a unidade de tempo (dias, segundos, etc.) e a sintaxe correta para realizar operações com datas.

Veja também:

- Documentação oficial da biblioteca 'datetime': https://docs.python.org/3/library/datetime.html
- Biblioteca 'arrow': https://arrow.readthedocs.io/en/stable/
- Como criar funções para calcular datas no futuro ou no passado: https://www.geeksforgeeks.org/adding-days-to-a-date-in-python/