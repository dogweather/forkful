---
date: 2024-01-26 00:56:28.520353-07:00
description: "Tratar erros em Python (ou em qualquer linguagem de programa\xE7\xE3\
  o) \xE9 prever o inesperado \u2013 \xE9 a arte de gerenciar com eleg\xE2ncia quando\
  \ as coisas d\xE3o errado\u2026"
lastmod: '2024-03-13T22:44:46.161720-06:00'
model: gpt-4-1106-preview
summary: "Tratar erros em Python (ou em qualquer linguagem de programa\xE7\xE3o) \xE9\
  \ prever o inesperado \u2013 \xE9 a arte de gerenciar com eleg\xE2ncia quando as\
  \ coisas d\xE3o errado\u2026"
title: Tratamento de erros
weight: 16
---

## O Quê e Por Quê?

Tratar erros em Python (ou em qualquer linguagem de programação) é prever o inesperado – é a arte de gerenciar com elegância quando as coisas dão errado no seu código. Fazemos isso para evitar falhas, guiar os usuários e tornar nossos programas robustos e confiáveis.

## Como fazer:

``` Python
# Bloco básico de try-except
try:
    # código arriscado
    numero = int(input("Digite um número: "))
except ValueError:
    # trata o erro
    print("Isso não é um número!")

# Especificando múltiplas exceções
try:
    # código que pode gerar diferentes exceções
    resultado = 10 / int(input("Digite um divisor: "))
except ZeroDivisionError:
    print("Ops! Não é possível dividir por zero.")
except ValueError:
    print("Eu preciso de um número, amigo.")

# Usando else e finally
try:
    numero = int(input("Digite um número para elevar ao quadrado: "))
except ValueError:
    print("Eu disse um número!")
else:
    # nenhum erro ocorreu
    print("O quadrado do seu número é:", numero**2)
finally:
    # sempre executa
    print("Obrigado por testar isso!")
```

Exemplo de saída ao digitar um número inválido para o primeiro bloco:
```
Digite um número: olá
Isso não é um número!
```

## Mergulho Profundo

Desde o início da programação, o tratamento de erros tem sido crucial. Abordagens iniciais eram rudimentares, como verificar condições antes de cada operação arriscada. A sintaxe de `try-except` do Python veio de um legado de tratamento de exceções em linguagens mais antigas como C++ e Java, simplificando o processo.

Quando você usa `try` em um bloco de código, o Python fica de olho em qualquer exceção. Se um erro aparece, o bloco `except` captura. Você pode ser específico quanto às exceções que captura ou pegar todas com um `except` vazio. No entanto, ser específico primeiro é a melhor abordagem – é preciso, e não uma rede de captura geral.

`else` e `finally` são extras neste conceito. O bloco `else` é executado se o bloco try estiver livre de erros. `finally` é o amigo confiável que executa não importa o quê – pense em operações de limpeza.

Alternativas? Certamente existem. Algumas linguagens usam códigos de retorno em vez de exceções. Você também pode encontrar instruções `with` para gerenciamento de recursos ou `assertions` que verificam condições durante o desenvolvimento. Mas quando falamos sobre estratégias sólidas de tratamento de erros, o modelo try-catch se destaca por sua legibilidade e estrutura.

## Veja Também

Aqui estão alguns bons recursos adicionais para se aprofundar ainda mais:

- Documentação oficial do Python sobre erros e exceções: [Documentação Python – Erros e Exceções](https://docs.python.org/3/tutorial/errors.html)
- Guia do Real Python sobre o assunto: [Real Python - O bloco try/except/else/finally](https://realpython.com/python-exceptions/)
- Uma discussão ponderada sobre as melhores práticas de tratamento de erros: [Stack Overflow – Como eu ignoro exceções corretamente?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
