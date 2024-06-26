---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:03.254487-07:00
description: "Como fazer: O Visual Basic for Applications (VBA) por si s\xF3 n\xE3\
  o suporta nativamente um shell interativo ou experi\xEAncia REPL como visto em linguagens\
  \ como\u2026"
lastmod: '2024-03-13T22:44:46.413905-06:00'
model: gpt-4-0125-preview
summary: "O Visual Basic for Applications (VBA) por si s\xF3 n\xE3o suporta nativamente\
  \ um shell interativo ou experi\xEAncia REPL como visto em linguagens como Python\
  \ ou JavaScript."
title: Usando uma shell interativa (REPL)
weight: 34
---

## Como fazer:
O Visual Basic for Applications (VBA) por si só não suporta nativamente um shell interativo ou experiência REPL como visto em linguagens como Python ou JavaScript. No entanto, você pode simular essa experiência até certo ponto usando a Janela Imediata no IDE (Ambiente de Desenvolvimento Integrado) do VBA.

**Acessando a Janela Imediata:**
1. Abra o IDE do VBA pressionando `Alt + F11` na sua aplicação do Office.
2. Se a Janela Imediata não estiver visível, você pode abri-la pressionando `Ctrl + G` ou selecionando-a no menu Exibir.

**Usando a Janela Imediata como um REPL:**
- Para executar uma linha de código, simplesmente digite-a na Janela Imediata e pressione Enter. Por exemplo:

```basic
Debug.Print 2 + 2
```

- Saída de Exemplo:
```
 4
```

- Você também pode chamar funções e sub-rotinas definidas em seus módulos:

```basic
Public Sub SayHello()
    Debug.Print "Olá, Mundo!"
End Sub
```

- E então, na Janela Imediata:
```basic
Call SayHello
```

- Saída de Exemplo:
```
 Olá, Mundo!
```

**Nota:** A Janela Imediata tem limitações. É excelente para testes rápidos e chamadas de função diretas, mas não suporta a definição de funções ou sub-rotinas diretamente dentro dela. Tarefas de depuração e programação complexas podem requerer o desenvolvimento completo do módulo.

## Aprofundamento
A Janela Imediata no VBA serve como o contraparte mais próximo de shells interativos encontrados em outros ecossistemas de programação, apesar de suas limitações. Historicamente, o VBA tem sido focado em estender as capacidades das aplicações do Microsoft Office por meio de scripts e macros em vez de desenvolvimento de software independente, o que pode explicar a ausência de um REPL completo.

Para tarefas que requerem testes interativos extensivos ou desenvolvimento de lógica complexa, outros ambientes de programação equipados com suporte nativo a REPL, como Python com seu IDLE, ou JavaScript com Node.js, podem oferecer alternativas melhores. Esses ambientes fornecem não apenas shells interativos, mas também instalações de programação, depuração e teste mais robustas.

A Janela Imediata fornece uma ferramenta inestimável para testar rapidamente expressões, executar funções e manipular diretamente objetos de aplicações do Office. Como tal, ocupa um nicho vital no processo de desenvolvimento do VBA, oferecendo uma imediatismo e conveniência incomparáveis por ciclos de compilação-execução-depuração mais tradicionais, embora com as restrições entendidas de seu escopo operacional.
