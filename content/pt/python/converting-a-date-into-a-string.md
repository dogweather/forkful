---
title:                "Python: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Ao trabalhar com datas em projetos de programação, frequentemente nos deparamos com a necessidade de convertê-las para strings. Isso pode acontecer para formatar a data de uma maneira específica ou para facilitar sua leitura e compreensão pelo usuário final. Neste artigo, vamos explorar como converter uma data em string usando a linguagem de programação Python.

## Como fazer:

Para converter uma data em uma string em Python, podemos utilizar o método `strftime()` da biblioteca `datetime`. Este método aceita um argumento de formatação que nos permite personalizar a forma como a data é exibida. Vamos dar uma olhada em alguns exemplos:

```Python
# Importando a biblioteca datetime
import datetime

# Obtendo a data atual
data_atual = datetime.datetime.now()

# Convertendo a data para uma string no formato ano-mes-dia
data_string = data_atual.strftime('%Y-%m-%d')

# Imprimindo a data como string
print(data_string)

# Saída: 2021-09-23
```

```Python
# Convertendo para o formato dia/mes/ano
data_string = data_atual.strftime('%d/%m/%Y')
print(data_string)

# Saída: 23/09/2021
```

Também é possível adicionar informações como o dia da semana ou o horário à string da data. Vamos ver um exemplo:

```Python
# Convertendo para o formato dia da semana, dia de mês de ano
data_string = data_atual.strftime('%A, %d de %B de %Y')
print(data_string)

# Saída: Quinta-feira, 23 de Setembro de 2021
```

Por fim, também podemos utilizar o método `strptime()` para converter uma string em data, caso seja necessário.

## Mergulho profundo:

A conversão de datas em string é amplamente utilizada em projetos de programação e é importante conhecer diferentes formatos e métodos para trabalhar com elas de forma eficaz. Além disso, ao personalizar a forma como a data é exibida, podemos criar uma melhor experiência para o usuário final. Também é possível trabalhar com diferentes fusos horários e converter a data para o fuso desejado antes de convertê-la para string. É importante lembrar que, ao trabalhar com datas, é essencial garantir que os dados sejam tratados corretamente para evitar problemas futuros.

## Veja também:

- [Documentação oficial do Python sobre formatação de datas](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
- [Guia completo sobre como trabalhar com datas em Python](https://www.geeksforgeeks.org/python-date-time/#strftime)
- [Tutorial para converter string em data em Python](https://www.programiz.com/python-programming/datetime/strptime)