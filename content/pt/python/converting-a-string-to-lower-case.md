---
title:                "Python: Convertendo uma string para minúsculas"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Converter uma string em caixa baixa (lower case) é uma tarefa comum em programação e pode ser útil em várias situações, como comparar strings sem levar em consideração a diferença entre maiúsculas e minúsculas.

## Como fazer

Para converter uma string em caixa baixa, podemos usar o método `lower()` em Python. Veja um exemplo abaixo:

```python
# Definir uma string
texto = "Olá, MUNDO!"

# Converter para caixa baixa
texto = texto.lower()

# Imprimir o resultado
print(texto)
```

**Saída:**

```python
olá, mundo!
```

## Mergulho Profundo

Ao usar o método `lower()`, é importante notar que ele retorna uma nova string convertida, mas não altera a string original. Por exemplo:

```python
texto = "Esse TEXTO vai ser CONVERtido"

texto_convertido = texto.lower()

print(texto) # Saída: Esse TEXTO vai ser CONVERtido
print(texto_convertido) # Saída: esse texto vai ser convertido
```

Além disso, o método `lower()` é sensível a localização (locale) e pode retornar resultados diferentes dependendo do idioma definido no seu sistema operacional. Por exemplo, em português, a letra "ç" é equivalente a "c" em caixa baixa, mas em inglês não é. Isso pode causar resultados inesperados em algumas situações.

## Veja também

- [Documentação do método `lower()` em Python](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Tutorial: Manipulação de strings em Python](https://www.python.org/dev/peps/pep-0616/)
- [Vídeo-aula: Python Strings and String Functions](https://www.youtube.com/watch?v=-uzs433zJTo)