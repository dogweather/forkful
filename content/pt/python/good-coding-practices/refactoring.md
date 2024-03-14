---
date: 2024-01-26 03:37:09.183893-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestruturar c\xF3digo de computador\
  \ existente\u2014mudando a fatora\xE7\xE3o\u2014sem alterar seu comportamento externo.\
  \ Programadores fazem\u2026"
lastmod: '2024-03-13T22:44:46.162668-06:00'
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestruturar c\xF3digo de computador\
  \ existente\u2014mudando a fatora\xE7\xE3o\u2014sem alterar seu comportamento externo.\
  \ Programadores fazem\u2026"
title: "Refatora\xE7\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Que?
Refatoração é o processo de reestruturar código de computador existente—mudando a fatoração—sem alterar seu comportamento externo. Programadores fazem isso para limpar o código, melhorar a legibilidade e facilitar a manutenção e extensão, tudo isso sem adicionar novas funcionalidades.

## Como fazer:
Suponha que você tenha um trecho de código que calcula e imprime a área e o perímetro de um retângulo dado seu comprimento e largura. Ele cumpre a tarefa, mas é repetitivo e um pouco bagunçado.

```python
# Versão Original
length = 4
width = 3

# Calcular área e perímetro
area = length * width
perimeter = 2 * (length + width)

print("Área:", area)
print("Perímetro:", perimeter)
```

Podemos refatorar isso encapsulando a funcionalidade em funções, o que torna o código mais organizado e reutilizável:

```python
# Versão Refatorada

def calcular_area(length, width):
    return length * width

def calcular_perimetro(length, width):
    return 2 * (length + width)

# uso
length = 4
width = 3

print("Área:", calcular_area(length, width))
print("Perímetro:", calcular_perimetro(length, width))
```

Ambos os trechos de código produzem o mesmo resultado:
```
Área: 12
Perímetro: 14
```

Mas a versão refatorada é mais limpa e separa as preocupações, tornando mais fácil atualizar um cálculo sem afetar o outro.

## Mergulho Profundo
A refatoração tem suas raízes nos primeiros dias da engenharia de software, quando os programadores perceberam que o código poderia—e deveria—ser melhorado mesmo se já estivesse "funcionando". O livro seminal de Martin Fowler "Refatoração: Melhorando o Design do Código Existente" articulou muitos princípios e técnicas fundamentais. Ele famosamente disse: "Qualquer tolo pode escrever código que um computador possa entender. Bons programadores escrevem códigos que humanos possam entender."

Alternativas à refatoração podem incluir reescrever o código do zero ou fazer pequenos ajustes sem melhoria sistemática. No entanto, a refatoração geralmente é mais econômica do que uma reescrita e menos arriscada do que modificações ad-hoc. Os detalhes de implementação podem ser específicos para cada paradigma de programação; no entanto, a programação orientada a objetos se presta particularmente bem à refatoração, especialmente com técnicas como extração de métodos (como nossas funções `calcular_area` e `calcular_perimetro`), inlining, movendo funcionalidades entre objetos, e renomeando métodos ou variáveis para clareza.

A refatoração em Python muitas vezes usa ferramentas como `PyCharm`, que tem capacidades de refatoração integradas, ou `rope`, uma biblioteca Python específica para refatoração. O uso cuidadoso do controle de versão, como `git`, durante a refatoração é fortemente aconselhado para acompanhar as mudanças incrementalmente.

## Veja Também
Para aqueles com fome de mais, mergulhe nos seguintes recursos:
- O livro de Martin Fowler: [Refatoração: Melhorando o Design do Código Existente](http://www.refactoring.com/)
- Refatoração em Python com `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- Documentação de refatoração do PyCharm: [Jetbrains PyCharm Refatorando Código Fonte](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refatoração e Padrões de Projeto](https://refactoring.guru/refactoring)
- Palestras sobre Código Limpo por Uncle Bob (Robert C. Martin): [Código Limpo - Uncle Bob / Lição 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
