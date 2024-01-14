---
title:    "Python: Imprimindo saída de depuração"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Por que imprimir saída de depuração é importante para programação Python?

Muitas vezes, ao escrever código em Python, nos deparamos com erros ou resultados inesperados. É nesse momento que a impressão de saída de depuração (debug output) pode ser uma ferramenta muito útil para nos ajudar a entender o que está acontecendo dentro do nosso código. Com a impressão de saída de depuração, podemos visualizar valores de variáveis, mensagens de erro e outras informações relevantes, facilitando a identificação e correção de possíveis problemas.

## Como fazer a impressão de saída de depuração em Python

Para imprimir saída de depuração em um código Python, utilizamos a função `print()`. Essa função aceita um ou mais argumentos e imprime o valor de cada um na tela. Vamos ver um exemplo simples:

```Python
numero = 10
print("O valor da variável numero é:", numero)
```

O resultado da execução deste código será:

```
O valor da variável numero é: 10
```

Podemos também imprimir o valor de uma variável ou expressão em uma mesma linha que outro texto, utilizando a formatação de string com f-strings, disponível a partir do Python 3.6. Veja o exemplo abaixo:

```Python
nome = "Maria"
sobrenome = "Souza"
print(f"Olá, meu nome é {nome} {sobrenome}.")
```

A saída será:

```
Olá, meu nome é Maria Souza.
```

## Aprofundando na impressão de saída de depuração

A impressão de saída de depuração é particularmente útil quando enfrentamos problemas relacionados a laços de repetição e condições, pois nos permite visualizar o valor de variáveis a cada passo do código. Além disso, é possível adicionar informações adicionais na mensagem de depuração, como mensagens explicativas e até mesmo marcar pontos específicos no nosso código:

```Python
for i in range(5):
    print(f"Valor atual de i: {i}") # mensagens com informações adicionais
    if i == 2:
        print("Cheguei a metade do laço!") # mensagem explicativa 
```

Saída:

```
Valor atual de i: 0
Valor atual de i: 1
Cheguei a metade do laço!
Valor atual de i: 2
Valor atual de i: 3
Valor atual de i: 4
```

Agora que você já sabe como imprimir saída de depuração em Python, aproveite essa ferramenta para facilitar a identificação e correção de problemas em seus códigos!

## Veja também

- [Documentação oficial do Python sobre a função `print()`](https://docs.python.org/pt-br/3/library/functions.html#print)
- [Artigo sobre formatação de strings com f-strings](https://realpython.com/python-f-strings/)
- [Tutorial sobre depuração de código em Python](https://realpython.com/python-debugging-pdb/)