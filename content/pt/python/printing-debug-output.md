---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Printar o debug output é a tarefa de mostrar a saída de depuração na tela de console para depurar o código. Os programadores fazem isso para localizar e resolver problemas (ou bugs), acompanhando de perto o comportamento e a saída do código.

## Como fazer:

É fácil introduzir a impressão de debug output em Python. A própria função `print()`, da linguagem Python, pode ser usada para tal.

Vamos conferir com um exemplo.

```Python
def calcular_soma(a, b):
    print(f"Somando {a} + {b}")  # debug output
    return a + b

resultado = calcular_soma(5, 10)
print(resultado)
```

Quando o código acima é executado, a saída de debug ("Somando 5 + 10") é impressa primeiro, antes do resultado da soma.

## Mergulho Profundo 

A depuração por meio da impressão começou no início dos dias de programação e continua sendo uma maneira eficaz de entender e resolver problemas de código. No entanto, Python possui módulos mais avançados, como `logging` e `pdb`, que oferecem funcionalidades de depuração mais robustas e flexíveis.

- **Logging**: Este módulo permite um controle mais apertado sobre o que é impresso, incluindo a capacidade de imprimir em diferentes níveis de severidade (INFO, DEBUG, ERROR) e para diferentes destinos (console, arquivos).
- **pdb**: Este é um depurador interativo mais avançado, permitindo que os programadores parem a execução do código, inspecionem o estado das variáveis e passem por códigos passo a passo.

Naturalmente, a escolha entre imprimir debug output simples e usar ferramentas mais avançadas depende do problema, da complexidade do código e da preferência pessoal do programador.

## Veja Também 

- [Python Debugging Techniques](https://realpython.com/python-debugging-pdb/): Um excelente aprofundamento nas diferentes formas de depuração em Python.
  

- [Python's Built-in print function](https://docs.python.org/3/library/functions.html#print): A documentação oficial da função print em Python.