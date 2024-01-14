---
title:                "Python: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em Python?

Quando estamos escrevendo código em Python, muitas vezes precisamos saber a data atual para realizar diferentes tarefas, como rastrear eventos ou calcular intervalos de tempo. Felizmente, há uma maneira fácil de obter a data atual em Python, tornando essa tarefa mais simples para nós, desenvolvedores. Neste artigo, vamos explorar como obter a data atual em Python e também mergulhar um pouco mais fundo neste conceito.

## Como fazer isso?

A maneira mais fácil de obter a data atual em Python é usando a biblioteca nativa `datetime`. Primeiro, precisamos importar a biblioteca em nosso código:

```Python
import datetime
```

Em seguida, podemos usar a função `date.today()` para obter a data atual:

```Python
data_atual = datetime.date.today()
```

Podemos, então, imprimir a data atual no formato desejado usando a função `strftime()` e passando o formato desejado como parâmetro:

```Python
print(data_atual.strftime("%d/%m/%Y"))
```

A saída será algo parecido com isso:

```
05/02/2021
```

Agora, se quisermos obter também a hora atual, podemos usar a função `datetime.now()` ao invés da `date.today()`:

```Python
hora_atual = datetime.datetime.now()
```

E novamente, podemos imprimir a hora atual no formato desejado:

```Python
print(hora_atual.strftime("%H:%M"))
```

A saída será algo parecido com isso:

```
12:23
```

## Explorando mais a fundo

Além de obter a data e hora atual, a biblioteca `datetime` também oferece opções para trabalhar com datas e horários passados e futuros, adicionar ou subtrair intervalos de tempo e até mesmo converter datas para diferentes fusos horários. É uma biblioteca muito útil para manipular datas e horários de forma precisa e eficiente em nossos projetos.

## Veja também

- [Documentação oficial do módulo datetime em Python](https://docs.python.org/3/library/datetime.html)
- [Python para todos: Módulo datetime](https://pythonparaestatistica.com.br/modulo-datetime-python/)
- [Manipulando datas e horários em Python](https://www.alura.com.br/artigos/manipulando-datas-e-horarios-com-python)