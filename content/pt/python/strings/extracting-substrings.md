---
date: 2024-01-20 17:46:32.869132-07:00
description: "Extrair substrings \xE9 o ato de selecionar partes espec\xEDficas de\
  \ uma string. Programadores fazem isso para manipular, analisar, ou verificar dados\
  \ dentro de\u2026"
lastmod: '2024-03-13T22:44:46.142844-06:00'
model: gpt-4-1106-preview
summary: "Extrair substrings \xE9 o ato de selecionar partes espec\xEDficas de uma\
  \ string."
title: Extraindo substrings
weight: 6
---

## O Que & Porquê?
Extrair substrings é o ato de selecionar partes específicas de uma string. Programadores fazem isso para manipular, analisar, ou verificar dados dentro de textos maiores.

## Como Fazer:
Extraímos substrings usando indexação e fatiamento em Python. Aqui estão exemplos:

```python
texto = "A arte de programar"

# Extrair a palavra 'arte'
substr = texto[2:6]
print(substr)  # Saída: arte

# Pegar os últimos 5 caracteres
ultimos_cinco = texto[-5:]
print(ultimos_cinco)  # Saída: amar

# Usando passo no fatiamento para pegar caracteres alternados
passo_dois = texto[::2]
print(passo_dois)  # Saída: Aat e rgaa
```

## Aprofundamento
Historicamente, a habilidade de manipular strings é fundamental em várias linguagens de programação e Python simplificou muito com uma sintaxe intuitiva desde suas primeiras versões. Além do fatiamento básico, há métodos como `.substring()` em outras linguagens, mas em Python utilizamos o fatiamento com colchetes. Na implementação, Python usa um objeto de string imutável, significando que cada vez que uma substring é criada, um novo objeto é feito na memória.

## Veja Também
- Documentação oficial de strings em Python: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str
- Pythontutor para visualizar a execução do código: http://www.pythontutor.com/visualize.html#mode=edit
- Tutorial W3Schools sobre strings em Python: https://www.w3schools.com/python/python_strings.asp
