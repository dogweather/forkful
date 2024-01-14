---
title:    "Python: Convertendo uma data em uma string"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que devemos converter uma data em uma string?

Existem várias situações em que precisamos trabalhar com datas em nossos códigos Python. No entanto, muitas vezes, essas datas estão em formato de objeto e não podem ser exibidas ou manipuladas facilmente. Converter uma data em uma string permite que nós a manipulemos de maneira mais conveniente e visualizemos sua representação de forma mais legível.

## Como converter uma data em uma string

Para converter uma data em uma string em Python, podemos usar o método `strftime()` da biblioteca `datetime`. Vejamos um exemplo:

```python
import datetime

data = datetime.date(2021, 10, 18)
texto = data.strftime("%d/%m/%Y")

print(texto)
```

**Output:** 18/10/2021

No código acima, importamos a biblioteca `datetime` e criamos um objeto `data` com a data que queremos converter. Em seguida, usamos o método `strftime()` com o padrão `%d/%m/%Y` para especificar o formato da string que queremos obter. O resultado é atribuído à variável `texto` e, por fim, é impresso na tela.

Podemos usar diferentes padrões para obter a representação das datas em diferentes formatos. Por exemplo:

- `%d`: Dia do mês (01 a 31)
- `%m`: Mês (01 a 12)
- `%Y`: Ano (quatro dígitos)
- `%y`: Ano (dois dígitos)
- `%B`: Nome do mês (janeiro a dezembro)
- `%b`: Abreviação do mês (jan a dez)
- `%A`: Dia da semana (segunda a domingo)
- `%a`: Abreviação do dia da semana (seg a dom)

Podemos combinar esses padrões de acordo com o formato que desejamos para a nossa data. Por exemplo, se quisermos exibir a data no formato "24 de julho de 2021", podemos usar o padrão `%d de %B de %Y`.

## Aprofundando-se

Para entender melhor a lógica por trás da conversão de uma data em uma string, é importante conhecer o módulo `time` e o método `strftime()` da biblioteca `datetime`. O módulo `time` é responsável por medir o tempo, enquanto o método `strftime()` da biblioteca `datetime` permite que formatemos uma data de acordo com nossas necessidades.

Além disso, é importante lembrar que a função `strftime()` só pode ser usada para converter uma data em uma string, não é possível fazer o contrário (converter uma string em uma data).

## Veja também

- Documentação oficial do Python sobre `datetime`: https://docs.python.org/3/library/datetime.html
- Tutorial sobre formatação de datas em Python: https://realpython.com/python-datetime/
- Vídeo explicando a conversão de datas em strings em Python: https://www.youtube.com/watch?v=9Alc7dl0ssQ