---
title:                "Python: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por que usar o Python para substituir texto?

A tarefa de substituir texto em um grande volume de dados pode ser uma tarefa tediosa e demorada. Felizmente, o Python oferece uma maneira eficiente e simples de realizar essa tarefa. Com as ferramentas certas, você poderá substituir texto em grandes arquivos de maneira rápida e eficiente.

# Como fazer isso com o Python

Para substituir texto utilizando o Python, você precisará usar a função `replace()` em uma string. Por exemplo, se você quiser substituir todas as ocorrências da palavra "casa" pela palavra "apartamento" em um arquivo de texto, você pode fazer da seguinte forma:

```Python
arquivo = "meu_arquivo.txt"
texto = open(arquivo).read()
novo_texto = texto.replace("casa", "apartamento")
print(novo_texto)
```

No exemplo acima, a função `replace()` irá substituir todas as ocorrências da palavra "casa" pela palavra "apartamento" no arquivo de texto. Em seguida, o resultado será impresso na tela.

# Aprofundando no assunto

Além de simplesmente substituir palavras, o Python também oferece recursos para substituir expressões regulares, que podem ser úteis em casos mais complexos. Por exemplo, se você quiser substituir todas as ocorrências de números por "#num", você pode usar o módulo `re` da seguinte maneira:

```Python
import re
arquivo = "meu_arquivo.txt"
texto = open(arquivo).read()
novo_texto = re.sub(r'\d+', '#num', texto)
print(novo_texto)
```

Neste exemplo, a função `sub()` do módulo `re` irá substituir todos os números encontrados na string por "#num". Além disso, você pode especificar opções adicionais para substituição, como a escolha de substituir somente o primeiro número encontrado ou substituir somente números que estejam entre certos limites.

# Veja também

- [Documentação oficial do Python para a função replace()](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutorial para usar expressões regulares no Python](https://realpython.com/regex-python/)

Esperamos que este artigo tenha sido útil para entender como utilizar o Python para substituir texto. Com a combinação certa de funções e módulos, você poderá realizar essa tarefa de maneira muito mais eficiente e rápida. Experimente e descubra como o Python pode facilitar suas tarefas de manipulação de texto!