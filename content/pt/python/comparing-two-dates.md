---
title:    "Python: Comparando duas datas"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar duas datas é uma tarefa comum em programação, e pode ser útil em diversos cenários, como por exemplo, encontrar a diferença de dias entre duas datas, verificar se uma data é anterior ou posterior a outra, ou até mesmo validar se uma data está dentro de um determinado intervalo.

## Como fazer em Python

Para comparar duas datas em Python, podemos utilizar o pacote `datetime` da biblioteca padrão. Primeiramente, precisamos importar o pacote:

```python
import datetime
```

Em seguida, podemos criar duas variáveis do tipo `datetime` com as datas que queremos comparar, utilizando o formato `ano-mês-dia`:

```python
data1 = datetime.datetime(2021, 9, 20)
data2 = datetime.datetime(2021, 9, 25)
```

Agora, podemos utilizar operadores lógicos como o `>` (maior que) e o `<` (menor que) para comparar as duas datas:

```python
data1 > data2  # retorna False
data1 < data2  # retorna True
```

Outra opção é utilizar o método `compare()` do objeto `datetime`:

```python
data1.compare(data2)  # retorna -1 (data1 é menor que data2)
```

Além disso, podemos utilizar o método `timedelta()` para encontrar a diferença entre as duas datas em dias:

```python
diferenca = data2 - data1
diferenca.days  # retorna 5 (diferença de 5 dias)
```

## Aprofundando mais na comparação de datas

Existem outras formas de comparar datas em Python, como utilizando o pacote `dateutil`, que permite uma maior flexibilidade com diferentes formatos de datas.

Também é importante mencionar que ao comparar datas em Python, os valores de hora, minuto, segundo e microsegundo também são levados em consideração. Portanto, é importante se atentar a esses detalhes antes de fazer uma comparação precisa.

Outro ponto a ser destacado é que devemos ter o cuidado de sempre validar as datas de entrada antes de realizar uma comparação, para evitar possíveis erros no código.

## Veja também

- [Documentação oficial do pacote datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial sobre manipulação de datas em Python](https://realpython.com/python-datetime/)
- [Comparando datas em diferentes formatos com dateutil](https://dateutil.readthedocs.io/en/stable/differences.html)