---
title:                "Convertendo uma data em uma string"
html_title:           "Python: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Convertendo uma data em uma string é o processo de transformar uma data em um formato legível para humanos, que usa palavras e símbolos em vez de números. Os programadores geralmente fazem isso para facilitar a visualização e transmissão de informações, já que as datas em formato string são mais compreensíveis do que as datas em formato numérico.

## Como fazer:

```python
# Importando o módulo datetime
import datetime

# Criando um objeto de data
data = datetime.date(2021, 7, 15)

# Convertendo a data em uma string
data_string = data.strftime("%d/%m/%Y")

# Imprimindo o resultado
print(data_string)

# Output: 15/07/2021
```

## Aprofundando:

- Contexto histórico: Antes do advento dos computadores, as datas costumavam ser escritas em formato string, usando palavras em vez de números para indicar o dia, mês e ano. Com o crescimento da tecnologia, foi necessário criar um formato numérico para facilitar a comunicação e armazenamento de informações.
- Alternativas: Além de converter datas em strings, também é possível converter em outros formatos, como JSON ou XML. No entanto, o formato de string é amplamente utilizado e compreendido por humanos, tornando-o uma escolha popular entre os programadores.
- Detalhes de implementação: Em Python, existem duas maneiras principais de converter uma data em string: `strftime()` e `strptime()`. A primeira permite especificar o formato da string resultante, enquanto a segunda usa o formato padrão do sistema. É importante ter atenção aos formatos para garantir que a data seja exibida corretamente.

## Veja também:

- Documentação oficial do módulo datetime em Python: https://docs.python.org/3/library/datetime.html
- Dicas para trabalhar com datas em Python: https://www.programiz.com/python-programming/datetime
- Tutorial de formatação de datas em Python: https://www.w3schools.com/python/python_datetime.asp