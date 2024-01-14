---
title:    "Python: Imprimindo saída de depuração"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

##Por que

Quando estamos escrevendo código em Python, às vezes podemos nos deparar com erros difíceis de solucionar. Nesses casos, pode ser útil usar a técnica de imprimir saídas de debug para entender melhor o que está acontecendo no nosso programa e encontrar a fonte do problema.

##Como fazer

Existem várias maneiras de imprimir saídas de debug em Python, mas a mais comum é usando a função `print()`. Vamos ver um exemplo simples:

```Python 
x = 10 
y = 5 
soma = x + y 
print("A soma de", x, "e", y, "é:", soma) 
```

Ao executar esse código, a saída será:

```
A soma de 10 e 5 é: 15
```

Podemos também usar a formatação de string para deixar nosso código mais organizado e legível:

``` Python 
x = 10 
y = 5 
soma = x + y 
print(f"A soma de {x} e {y} é: {soma}.") 
``` 

A saída continuará sendo a mesma, mas dessa forma temos um código mais enxuto e fácil de entender.

##Aprofundando no assunto 

Ao usar a função `print()` para debugar nosso código, é importante lembrar que podemos imprimir qualquer tipo de dado, desde variáveis e números até listas e dicionários. Além disso, podemos combinar múltiplos elementos em uma única linha de saída, usando a vírgula para separá-los.

Outra dica importante é utilizar a opção `end` para especificar o caractere final de cada linha de saída. Por padrão, o `end` é igual a `\n`, que gera uma quebra de linha, mas podemos alterá-lo para, por exemplo, um espaço em branco:

```Python 
print("Olá", end=" ") 
print("mundo!") 
``` 

A saída será `Olá mundo!`, sem a quebra de linha entre as duas palavras.

E se quisermos imprimir valores numéricos com uma quantidade específica de casas decimais? Podemos fazer isso usando a função `round()` dentro do `print()`, por exemplo:

```Python 
a = 1.23456 
b = 2.5 
print(f"Resultado: {round(a/b, 2)}") 
``` 

A saída será `Resultado: 0.49`.

##Veja também 

- [Documentação oficial do Python sobre a função `print()`](https://docs.python.org/3/library/functions.html#print) 
- [Artigo sobre como usar a função `print()` para debugar seu código em Python](https://medium.com/@permutans/debbuging-in-python-print-with-style-4e33101f2bed) 
- [Vídeo tutorial sobre como imprimir saídas de debug no Python](https://www.youtube.com/watch?v=YDlY8VaLQyM)