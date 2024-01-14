---
title:    "Python: Convertendo uma data em uma string"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string em Python?

Converter uma data em uma string é uma tarefa comum ao lidar com dados no Python. Isso pode ser útil ao formatar saídas em um programa, armazenar datas em um banco de dados ou para várias outras tarefas. Neste artigo, vamos explorar como fazer essa conversão de forma eficiente e eficaz.

## Como fazer a conversão

A conversão de uma data em uma string é simples de ser realizada no Python, graças às bibliotecas integradas `datetime` e `strftime`. Primeiro, precisamos importar essas bibliotecas em nosso código:

```Python
import datetime
from datetime import strftime
```

Em seguida, podemos criar um objeto de data usando a função `datetime.date()` e especificando o ano, mês e dia:

```Python
data = datetime.date(2021, 12, 25)
```

Para converter essa data em uma string, usamos a função `strftime()` e especificamos o formato desejado, como no exemplo abaixo:

```Python
data_string = data.strftime("%d/%m/%Y")
```

O código acima irá converter a data em uma string com o formato "dia/mês/ano", resultando em "25/12/2021".

## Mais detalhes sobre a conversão

Existem muitas opções diferentes de formatação que podem ser usadas ao converter uma data em uma string no Python. Alguns exemplos incluem:

- `%d`: dia (01 - 31)
- `%m`: mês (01 - 12)
- `%b`: mês abreviado (jan - dez)
- `%B`: mês por extenso (janeiro - dezembro)
- `%y`: ano com dois dígitos (21)
- `%Y`: ano com quatro dígitos (2021)

Você também pode combinar vários formatos, adicionando símbolos como barras (/) ou hifens (-) entre cada elemento.

Além disso, é possível adicionar informações adicionais como o dia da semana ou o horário na conversão. Não há limites para a criatividade na formatação da string de data!

## Veja também

Para mais informações sobre as bibliotecas `datetime` e `strftime`, recomendamos os seguintes links:

- [Documentação oficial do Python sobre a biblioteca `datetime`](https://docs.python.org/3/library/datetime.html)
- [Tutorial da Real Python sobre formatação de datas em Python](https://realpython.com/python-datetime/)
- [Lista completa de formatos de data para a função `strftime`](https://strftime.org/)

Esperamos que este artigo tenha sido útil a você para aprender como converter uma data em uma string no Python. Não hesite em experimentar diferentes formatos e compartilhar suas descobertas nos comentários. Boa codificação!