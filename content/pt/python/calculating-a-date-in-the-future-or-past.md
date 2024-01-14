---
title:    "Python: Calculando uma data no futuro ou passado"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que

A programação é uma ferramenta poderosa que pode nos ajudar a simplificar tarefas. Calcular uma data no futuro ou passado pode ser extremamente útil em muitos projetos, economizando tempo e evitando erros humanos.

## Como Fazer

Para calcular uma data no futuro ou passado em Python, primeiro precisamos importar o módulo `datetime`. Em seguida, usamos a função `timedelta` para definir a quantidade de tempo que queremos adicionar ou subtrair da data atual. Veja um exemplo abaixo:

```Python
import datetime

data_atual = datetime.date.today()
dias_para_adicionar = datetime.timedelta(days=7)

data_futura = data_atual + dias_para_adicionar
print(data_futura)
```

Neste exemplo, definimos `dias_para_adicionar` como 7, o que significa que estamos calculando uma data 7 dias no futuro. A saída será a data formatada como "ano-mês-dia", por exemplo, "2020-08-30".

Podemos usar esta mesma lógica para calcular uma data no passado, basta alterar o sinal de adição para subtração:

```Python
import datetime

data_atual = datetime.date.today()
dias_para_subtrair = datetime.timedelta(days=30)

data_passada = data_atual - dias_para_subtrair
print(data_passada)
```

Neste exemplo, estamos calculando uma data 30 dias no passado. A saída será "2020-07-31".

## Uma Mergulho Profundo

Além de adicionar ou subtrair dias, também é possível usar a função `timedelta` para adicionar ou subtrair horas, minutos e segundos. Por exemplo:

```Python
import datetime

hora_atual = datetime.datetime.now()
minutos_para_adicionar = datetime.timedelta(minutes=15)

hora_futura = hora_atual + minutos_para_adicionar
print(hora_futura)
```

Neste exemplo, estamos adicionando 15 minutos à hora atual. A saída será a hora formatada como "hora:minutos:segundos", por exemplo, "13:00:00".

Também é importante notar que a função `timedelta` aceita valores negativos, o que nos permite subtrair o tempo desejado.

## Veja Também

- [Documentação do módulo datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial de programação em Python](https://www.python.org/about/gettingstarted/)