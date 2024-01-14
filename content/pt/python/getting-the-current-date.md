---
title:    "Python: Obtendo a data atual"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que obter a data atual é importante em programação?

Obter a data atual pode ser uma tarefa simples em Python, mas é uma ferramenta importante para muitos programadores. É útil em várias situações, como em sistemas de registros de dados ou para criar cronogramas e relatórios.

## Como fazer isso em Python

Existem várias maneiras de obter a data atual em Python, mas a mais comum é usando o módulo `datetime`. Veja um exemplo simples com a saída correspondente:

```Python
import datetime
data_atual = datetime.date.today()
print(data_atual)
```

Saída:
```
2021-10-12
```

Você também pode personalizar o formato da data usando o método `strftime()` e especificando os elementos que você deseja incluir. Neste exemplo, vamos adicionar o dia da semana:

```Python
import datetime
data_atual = datetime.date.today()
dia_da_semana = data_atual.strftime("%A")
print(dia_da_semana)
```

Saída:
```
Terça-feira
```

Há também a opção de obter a data com informações extras, como a hora atual. Veja um exemplo com a saída correspondente:

```Python
import datetime
data_hora_atual = datetime.datetime.now()
print(data_hora_atual)
```

Saída:
```
2021-10-12 14:25:23.789132
```

## Aprofundando-se no assunto

Além do módulo `datetime`, há outras formas de obter a data atual em Python, como usando a biblioteca `arrow` ou a função `time` do módulo padrão `time`. Além disso, é importante entender o conceito de fuso horário e como ele pode afetar a data e hora apresentadas.

## Veja também

- Documentação do módulo `datetime`: https://docs.python.org/3/library/datetime.html
- Tutorial sobre o módulo `datetime`: https://www.w3schools.com/python/python_datetime.asp
- Exemplo de uso do módulo `arrow`: https://docs.python.org/3/library/datetime.html
- Tutorial sobre a função `time`: https://www.programiz.com/python-programming/datetime/current-datetime