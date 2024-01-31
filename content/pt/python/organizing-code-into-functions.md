---
title:                "Organizando o código em funções"
date:                  2024-01-26T01:11:55.080327-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Organizar o código em funções se trata de dividir o código em blocos reutilizáveis com propósitos específicos. Fazemos isso para tornar o código mais limpo, fácil de ler, depurar e atualizar.

## Como fazer:
Vamos supor que você está escrevendo um script para calcular o quadrado e o cubo de um número. Sem funções, é uma bagunça de repetições:

```Python
num = 4
quadrado = num * num
cubo = num * num * num
print(f"Quadrado: {quadrado}, Cubo: {cubo}")

num = 5
quadrado = num * num
cubo = num * num * num
print(f"Quadrado: {quadrado}, Cubo: {cubo}")
```
Saída:
```
Quadrado: 16, Cubo: 64
Quadrado: 25, Cubo: 125
```

Com funções, fica mais organizado:

```Python
def quadrado(n):
    return n * n

def cubo(n):
    return n ** 3

num = 4
print(f"Quadrado: {quadrado(num)}, Cubo: {cubo(num)}")

num = 5
print(f"Quadrado: {quadrado(num)}, Cubo: {cubo(num)}")
```
Saída:
```
Quadrado: 16, Cubo: 64
Quadrado: 25, Cubo: 125
```

## Mais Detalhes
Antigamente, quando os programas eram simples, podia-se passar apenas escrevendo uma lista de instruções. Mas à medida que o software foi ficando mais complexo, os desenvolvedores perceberam que estavam reescrevendo o mesmo código repetidamente. Olá, funções—blocos reutilizáveis de código que realizam uma única ação.

Alternativas às funções incluem classes (agrupando funções com os dados sobre os quais operam) e código inline (inteligência exatamente onde você precisa, mas arriscado para tarefas complexas). Em termos de implementação, o truque não é apenas criar funções, mas fazê-las realizar uma coisa bem—pense no princípio da responsabilidade única. As funções também devem idealmente ser sem estado, significando sem surpresas com os dados que entram ou saem.

## Veja Também
- Os tutoriais oficiais de Python sobre funções: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Código Limpo' de Robert C. Martin, para princípios sobre como escrever funções limpas.
- 'Refatoração: Aperfeiçoando o Projeto de Código Existente' de Martin Fowler, que inclui exemplos de organização de código.
