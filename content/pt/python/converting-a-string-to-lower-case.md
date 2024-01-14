---
title:    "Python: Convertendo uma string para minúsculas"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que converter uma string para minúsculas em Python

Ao trabalhar com dados de texto em Python, pode ser útil converter uma string para letras minúsculas. Isso permite que você normalize seus dados e torne suas análises mais precisas.

## Como fazer

Existem algumas maneiras de converter uma string para minúsculas em Python. Uma delas é usando o método `lower()`, que retorna uma cópia da string em letras minúsculas. Veja um exemplo abaixo:

```Python
texto = "Olá MUNDO!"
print(texto.lower())
```
A saída será: "olá mundo!"

Outra maneira é usando a função `str.lower()`, que também retorna a string em minúsculas. Veja um exemplo:

```Python
texto = "Este TEXTO Será CoNvErtIdO para Minúsculas"
print(str.lower(texto))
```
A saída será: "este texto será convertido para minúsculas"

## Mergulho Profundo

É importante notar que, ao converter uma string para minúsculas, qualquer caractere que não seja uma letra permanecerá inalterado. Isso inclui pontuação e números. Além disso, a conversão para minúsculas é sensível ao idioma. Por exemplo, em português, a letra "I" maiúscula se torna "i" minúscula, enquanto em turco, ela se torna "ı" minúscula.

Você também pode usar o método `replace()` para substituir caracteres maiúsculos por minúsculos em uma string. Veja um exemplo:

```Python
texto = "Viva o BRASIL!"
print(texto.replace("V", "v").replace("B", "b"))
```
A saída será: "viva o brasil!"

Por fim, é importante ressaltar que, quando você converte uma string para minúsculas, está criando uma nova string, já que as strings em Python são imutáveis. Isso significa que a string original permanecerá inalterada.

# Veja também

- [Documentação oficial do Python sobre o método `lower()`](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Tutorial sobre strings em Python](https://realpython.com/python-strings/)
- [Vídeo do PyLadies sobre manipulação de strings em Python](https://www.youtube.com/watch?v=OvJ8YFA8VgM)