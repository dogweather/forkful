---
date: 2024-01-27 20:34:22.043427-07:00
description: "Como fazer: Lua oferece suporte embutido para gerar n\xFAmeros aleat\xF3\
  rios atrav\xE9s da fun\xE7\xE3o `math.random`. Esta fun\xE7\xE3o pode ser usada\
  \ de m\xFAltiplas maneiras,\u2026"
lastmod: '2024-03-13T22:44:46.705102-06:00'
model: gpt-4-0125-preview
summary: "Lua oferece suporte embutido para gerar n\xFAmeros aleat\xF3rios atrav\xE9\
  s da fun\xE7\xE3o `math.random`."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como fazer:
Lua oferece suporte embutido para gerar números aleatórios através da função `math.random`. Esta função pode ser usada de múltiplas maneiras, dependendo do resultado desejado:

1. **Gerando um número flutuante aleatório entre 0 e 1:**

```Lua
print(math.random())
```

A saída de exemplo pode ser `0.13117647051304`. Cada execução produz um valor diferente.

2. **Gerando um número inteiro aleatório dentro de uma faixa especificada:**

Para produzir um número inteiro aleatório entre duas margens, inclusivas, você primeiro precisa definir a semente usando `math.randomseed(os.time())` para variabilidade, depois chamar `math.random` com dois argumentos:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- Gera um número inteiro aleatório entre 1 e 10
```

A saída de exemplo poderia ser `7`. Novamente, a saída varia com cada execução.

É crucial definir a semente com `math.randomseed`, pois, sem isso, `math.random` poderia gerar a mesma sequência de números cada vez que um programa é executado. Tipicamente, o uso do tempo atual, `os.time()`, garante sequências diferentes por execução.

## Aprofundamento
O mecanismo subjacente à geração de números aleatórios em Lua (e na maioria das linguagens de programação) não é verdadeiramente aleatório, mas pseudorrandômico, gerado por um algoritmo. Esses geradores de números pseudorrandômicos (PRNGs) são determinísticos e requerem um valor semente para iniciar a sequência de geração de números. A escolha da semente é crucial para a qualidade da aleatoriedade, o que é o motivo pelo qual usar o tempo atual é uma prática comum.

Historicamente, as capacidades de geração de números aleatórios do Lua evoluíram. Versões anteriores dependiam da função `rand()` da biblioteca padrão C, que variava em qualidade e desempenho entre implementações. A versão atual do Lua aprimora isso possivelmente usando mecanismos mais robustos dependendo da plataforma subjacente, oferecendo maior consistência e utilidade na geração de números aleatórios.

Para projetos que requerem aleatoriedade a nível criptográfico, a funcionalidade embutida do Lua pode não ser suficiente devido à natureza determinística dos PRNGs. Nesses casos, programadores muitas vezes recorrem a bibliotecas externas ou APIs específicas do sistema que podem fornecer números aleatórios não determinísticos adequados para aplicações de alta segurança.
