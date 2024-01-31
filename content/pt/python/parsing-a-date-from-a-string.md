---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:38:12.605980-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Pegar uma data de um texto é como tirar um coelho da cartola: você tem uma string e quer transformá-la numa data que seu programa entende para poder usar em comparações, cálculos ou armazenamento. Programadores fazem isso porque as datas frequentemente vêm em formatos variados, como entradas de usuário ou arquivos externos, e precisam ser normalizadas para um uso consistente no software.

## Como Fazer:
Python tem uma biblioteca chamada `datetime` que é a varinha mágica para lidar com datas e tempos. Veja como isso funciona:

```Python
from datetime import datetime

# Parse de uma data em formato conhecido
data_string = "01/04/2023"
data_formato = "%d/%m/%Y"
data_objeto = datetime.strptime(data_string, data_formato)

print(data_objeto) # Saída: 2023-04-01 00:00:00

# Parse de uma data em formato ISO 8601
data_iso_string = "2023-04-01T15:30:00"
data_iso_objeto = datetime.fromisoformat(data_iso_string)

print(data_iso_objeto) # Saída: 2023-04-01 15:30:00
```

## Mergulho Profundo:
Nas eras paleolíticas da computação, como na década de 70, parsear datas era trabalho manual e cheio de armadilhas. Com a evolução das linguagens, surgiram bibliotecas para isso. Em Python, a biblioteca `datetime` é a forma padrão desde a versão 2.3, substituindo a módulos mais antigos como o `time`.

Existem alternativas como `dateutil`, que pode lidar com formatos mais malucos e flexíveis. Já para quem precisa de performance, `ciso8601` é um parser de data ISO 8601 escrito em C, bem mais rápido.

Em termos de implementação, `datetime.strptime()` trabalha com uma string de formato que você define. Por exemplo, `%d` corresponde ao dia do mês e `%Y` ao ano com quatro dígitos. Essa string de formato precisa ser ajustada exatamente ao formato de entrada para que a conversão ocorra sem erros.

## Veja Também:
- Documentação oficial do `datetime`: https://docs.python.org/3/library/datetime.html
- `dateutil`, para parsear formatos mais complexos: https://dateutil.readthedocs.io
- `ciso8601`, se você quiser performance pura: https://github.com/closeio/ciso8601
