---
title:    "Python: Obtendo a data atual"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa essencial em muitos projetos de programação. Ela pode ser utilizada para rastrear eventos importantes, armazenar dados em bancos de dados e até mesmo para exibir informações relevantes aos usuários. Além disso, entender como obter a data atual pode melhorar suas habilidades de programação em Python.

## Como fazer:

Para obter a data atual em Python, usamos o módulo "datetime". Primeiro, importamos o módulo com o comando "import datetime". Em seguida, usamos a função "datetime.now()" para obter a data atual em formato de objeto. Por fim, podemos utilizar os métodos do objeto para formatar a data da maneira desejada.

Segue abaixo um exemplo de código e a saída correspondente:

```Python
import datetime
data_atual = datetime.now()
print(data_atual)
# Output: 2021-08-03 14:32:05.786171
```

Podemos também especificar o formato da data utilizando o método "strftime()", como no exemplo abaixo:

```Python
import datetime
data_atual = datetime.now()
data_formatada = data_atual.strftime('%d/%m/%Y')
print(data_formatada)
# Output: 03/08/2021
```

## Mergulho profundo:

O módulo "datetime" oferece muitas outras funcionalidades para trabalhar com datas e horários, como calcular a diferença entre duas datas, adicionar ou subtrair dias, horas ou segundos, entre outras. Além disso, também é possível criar datas específicas utilizando o método "datetime()", passando os valores desejados para ano, mês, dia, hora, minuto e segundo.

## Veja também:

- [Documentação do módulo datetime para Python](https://docs.python.org/3/library/datetime.html)
- [Tutorial de Python para iniciantes](https://realpython.com/python-beginner-guide/)
- [Outros módulos úteis para manipulação de datas em Python](https://medium.com/python-pandemonium/date-and-time-manipulation-in-python-29a5937c1104)

Obrigado por ler este artigo e espero que tenha sido útil para você aprender um pouco mais sobre como obter a data atual em Python!