---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:40.129537-07:00
description: "Como fazer: Ao contr\xE1rio de algumas linguagens que possuem interpola\xE7\
  \xE3o de string embutida, VBA requer uma abordagem mais manual tipicamente usando\
  \ o\u2026"
lastmod: '2024-03-13T22:44:46.396411-06:00'
model: gpt-4-0125-preview
summary: "Ao contr\xE1rio de algumas linguagens que possuem interpola\xE7\xE3o de\
  \ string embutida, VBA requer uma abordagem mais manual tipicamente usando o operador\
  \ `&` ou a fun\xE7\xE3o `Format` para embutir vari\xE1veis em strings."
title: Interpolando uma string
weight: 8
---

## Como fazer:
Ao contrário de algumas linguagens que possuem interpolação de string embutida, VBA requer uma abordagem mais manual tipicamente usando o operador `&` ou a função `Format` para embutir variáveis em strings. Abaixo estão exemplos que mostram esses métodos:

**Usando o operador `&`:**

```vb
Dim userName As String
Dim userScore As Integer

userName = "Alice"
userScore = 95

' Concatenando strings e variáveis
Dim message As String
message = "Parabéns, " & userName & "! Sua pontuação é " & userScore & "."
Debug.Print message
```
**Saída:**
```
Parabéns, Alice! Sua pontuação é 95.
```

**Usando a função `Format`:**

Para cenários mais complexos, como incluir números ou datas formatados, a função `Format` é inestimável.

```vb
Dim currentDate As Date
currentDate = Date

Dim formattedMessage As String
formattedMessage = "Hoje é " & Format(currentDate, "MMMM dd, yyyy") & ". Tenha um ótimo dia!"
Debug.Print formattedMessage
```

**Saída:**
```
Hoje é 15 de abril de 2023. Tenha um ótimo dia!
```

## Aprofundando
A interpolação de string como conhecida em linguagens de programação modernas como Python ou JavaScript não existe diretamente em VBA. Historicamente, desenvolvedores de VBA tinham que depender da concatenação usando `&` ou utilizar a função `Format` para inserir valores em strings, muitas vezes tornando o processo trabalhoso para strings complexas ou necessitando de formatação precisa. Essa diferença enfatiza a era de origem do VBA e seu foco na simplicidade direta em detrimento de algumas conveniências modernas.

No entanto, é essencial notar que, embora o VBA não ofereça interpolação de string embutida, o domínio do `&` para concatenações simples ou `Format` para cenários mais complexos permite uma manipulação de strings robusta e flexível. Para desenvolvedores vindos de linguagens com recursos nativos de interpolação de string, isso pode inicialmente parecer um retrocesso, mas esses métodos oferecem um nível de controle que, uma vez dominado, pode ser incrivelmente poderoso. Além disso, ao migrar para ambientes .NET mais recentes, os programadores encontrarão a interpolação de string como um recurso de primeira classe no VB.NET, fornecendo uma abordagem mais familiar e eficiente para criar strings dinâmicas. Em termos práticos, entender as diferenças e limitações no VBA pode ajudar muito na escrita de um código eficiente e legível e facilitar a transição para ambientes Visual Basic mais modernos, se necessário.
