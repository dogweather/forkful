---
title:    "Python: Usando expressões regulares"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que utilizar Expressões Regulares?

Expressões regulares são uma ferramenta poderosa para buscar e manipular padrões de texto em programas Python. Com elas, é possível realizar tarefas como validação de dados, extração de informações específicas e substituição de caracteres. Se você deseja otimizar seu código e automatizar tarefas repetitivas, aprender Expressões Regulares é fundamental.

## Como Utilizar Expressões Regulares em Python

Para utilizar Expressões Regulares em Python, é necessário importar o módulo `re`. Este módulo contém várias funções que permitem criar e operar com expressões regulares.

Vamos supor que temos uma lista de e-mails e queremos validar se eles estão em um formato correto. Podemos utilizar a função `match()` para verificar se um e-mail começa com um conjunto de caracteres específicos e possui um "@" e um domínio válido.

```
import re

emails = ["joao@gmail.com", "maria@outlook.com", "pedro@hotmail.com"]

for email in emails:
    if re.match(r"[A-Za-z0-9]+@([A-Za-z0-9]+\.)+[A-Za-z]+", email):
        print(f"{email} é um e-mail válido!")
    else:
        print(f"{email} é um e-mail inválido!")
```

Neste exemplo, utilizamos uma expressão regular que verifica se o e-mail começa com uma combinação de letras e números, seguido de um "@" e um domínio formado por letras e números, seguido de um ponto e mais letras. Com isso, podemos validar facilmente se os e-mails da lista estão em um formato correto.

## Aprofundando-se em Expressões Regulares

As Expressões Regulares em Python seguem uma sintaxe específica para definir padrões de texto. Alguns dos símbolos comumente utilizados são:

- `.` : representa qualquer caractere.
- `+` : representa a ocorrência de um ou mais caracteres.
- `*` : representa a ocorrência de zero ou mais caracteres.
- `[]` : utiliza-se para criar uma lista de possíveis caracteres.
- `()` : é utilizado para agrupar padrões.

É importante lembrar que o uso de símbolos e metacaracteres pode variar dependendo da necessidade e do padrão que se deseja buscar.

Também é possível utilizar outras funções como `search()` e `findall()` para buscar padrões em textos mais complexos. Para uma lista completa das funções disponíveis e suas aplicações, consulte a documentação oficial do módulo `re` em [Python.org](https://docs.python.org/pt-br/3/library/re.html).

## Veja também

- [Expressões Regulares em Python: O Guia Definitivo](https://www.devmedia.com.br/expressoes-regulares-em-python-o-guia-definitivo/40657)
- [Aprenda Python: Expressões Regulares](https://www.youtube.com/watch?v=K8L6KVGG-7o)
- [Documentação oficial do módulo `re` em Python.org](https://docs.python.org/pt-br/3/library/re.html)