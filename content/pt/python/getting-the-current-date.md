---
title:                "Obtendo a data atual."
html_title:           "Python: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que?

Antes de começarmos a explorar como obter a data atual em Python, é importante entendermos a importância dessa funcionalidade. Muitas vezes, em nossas rotinas de programação, precisamos lidar com datas, seja para registrar quando um evento ocorreu, para agendar tarefas ou para realizar cálculos envolvendo prazos. Portanto, ter uma maneira de obter a data atual de forma precisa e automatizada é extremamente útil e economiza tempo e esforço dos desenvolvedores.

## Como obter a data atual em Python

Agora que sabemos a importância de obter a data atual em nossos projetos, vamos mostrar como fazer isso em Python. Felizmente, a biblioteca padrão do Python já possui um módulo dedicado exclusivamente para gerenciar datas, chamado "datetime".

Para obter a data e hora atuais, podemos usar o método `now()` do objeto `datetime` dentro desse módulo. Vamos ver um exemplo:

```Python
import datetime

# armazenando a data atual em uma variável
data_atual = datetime.datetime.now()

# imprimindo a data formatada
print(f"A data atual é: {data_atual.day}/{data_atual.month}/{data_atual.year}")
```

Ao executar esse código, teremos como output: `A data atual é: 31/08/2021` (considerando que o dia atual seja 31 de agosto de 2021). Podemos também formatar a data de outras maneiras, como mostrando o nome do mês ou exibindo a hora junto com a data.

É importante notar que a data e hora obtidas serão baseadas no fuso horário do sistema em que o código está sendo executado. Portanto, se você estiver em um país diferente do seu servidor, por exemplo, a data e hora podem ser diferentes.

## Aprofundando-se

Caso você queira uma personalização maior da sua data atual, a biblioteca `datetime` oferece diversas opções altamente configuráveis. Por exemplo, você pode especificar um fuso horário diferente, adicionar ou subtrair dias ou horas, trabalhar com datas em diferentes formatos e muito mais. Consulte a documentação oficial para saber mais detalhes.

Além disso, também é possível criar e trabalhar com objetos de data específicos no passado ou futuro, utilizando os métodos `today()` e `replace()` da biblioteca `datetime`.

## Veja também

- [Documentação oficial do módulo datetime em Python](https://docs.python.org/3/library/datetime.html)
- [Artigo sobre como formatar datas em Python](https://pythonacademy.com.br/blog/manipulando-datas-com-python)
- [Tutorial para iniciantes sobre manipulação de datas em Python](https://towardsdatascience.com/python-datetime-manipulation-acfcfbbd26a9)