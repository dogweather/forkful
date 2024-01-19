---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo Caracteres Que Correspondem a Um Padrão em Python

## O que & Por quê?

Apagar caracteres que correspondem a um padrão é um método comum para manipular strings em Python. Os programadores usam para limpar ou formatar dados, de acordo com uma condição definida.

## Como fazer:

Apagar caracteres de uma string pode ser feito em Python de várias maneiras. Aqui estão alguns exemplos.

```Python
import re
  
# Exemplo 1
texto = "Olá, meu número de telefone é +1234567890."
print(re.sub("[^0-9]", "", texto))  # Saída: 1234567890

# Exemplo 2
mensagem = "!Wow! Este é o melhor?!.código##."
print(re.sub('[^A-Za-z0-9 ]+', '', mensagem))  # Saída: Wow Este é o melhor código
```

O primeiro exemplo remove tudo que não é um número na string, enquanto o segundo remove qualquer caráter que não seja uma letra ou um número.

## Mergulho Profundo

Este método de substituição de caracteres que correspondem a padrões específicos usa expressões regulares (regex), um recurso poderoso introduzido em linguagens de programação para manipular strings.

Existem maneiras alternativas de realizar a mesma tarefa, por exemplo,.concatenação de strings ou compreensões de listas. Mas, ao usar regex, estamos condensando a complexidade do nosso código.

Na implementação acima, a função `sub()` do módulo regex é usada. Ela substitui todas as ocorrências de caracteres que correspondem ao padrão por uma nova string (neste caso, uma string vazia '').

## Veja também 

Para mais detalhes sobre regex, você pode consultar a documentação oficial em [Python Docs](https://docs.python.org/3/library/re.html) e um passo a passo de regex em português pode ser encontrado nesse link: [Tutorial Regex](https://pythonhelp.wordpress.com/2013/08/29/expressoes-regulares-em-python/).