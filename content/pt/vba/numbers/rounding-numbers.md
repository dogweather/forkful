---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:57.014944-07:00
description: "Arredondar n\xFAmeros em programa\xE7\xE3o \xE9 sobre aproximar um n\xFA\
  mero ao seu n\xFAmero inteiro mais pr\xF3ximo ou a um certo n\xFAmero de casas decimais.\
  \ Programadores\u2026"
lastmod: '2024-03-11T00:14:20.100314-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros em programa\xE7\xE3o \xE9 sobre aproximar um n\xFA\
  mero ao seu n\xFAmero inteiro mais pr\xF3ximo ou a um certo n\xFAmero de casas decimais.\
  \ Programadores\u2026"
title: "Arredondamento de n\xFAmeros"
---

{{< edit_this_page >}}

## O Quê e Por Quê?

Arredondar números em programação é sobre aproximar um número ao seu número inteiro mais próximo ou a um certo número de casas decimais. Programadores arredondam números para simplificar figuras, melhorar a legibilidade, ou atender a critérios numéricos específicos em cálculos, especialmente em computações financeiras onde a precisão importa.

## Como Fazer:

No Visual Basic for Applications (VBA), o arredondamento pode ser alcançado usando várias funções, cada uma adequada para cenários específicos. Aqui estão as funções mais comumente usadas com exemplos:

1. **Função Round**:
   A função `Round` arredonda um número para um número especificado de dígitos.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Saída: 3.14
   MsgBox roundedNumber
   ```
   
2. **Funções Int e Fix**:
   Ambas as funções `Int` e `Fix` são usadas para arredondar números para baixo ao inteiro mais próximo, mas elas se comportam de maneira diferente com números negativos.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Saída: -4
   fixRounded = Fix(-3.14159)  ' Saída: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Funções Ceiling e Floor**:
   O VBA não possui as funções `Ceiling` e `Floor` incorporadas encontradas em outros idiomas. Para simular isso, use `Application.WorksheetFunction.Ceiling_Math` e `Application.WorksheetFunction.Floor_Math` para o Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Saída: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Saída: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Aprofundamento

A função `Round` no VBA é inerentemente diferente dos métodos de arredondamento em outros idiomas devido ao seu uso do **Arredondamento do Banqueiro**. O Arredondamento do Banqueiro arredonda para o número par mais próximo quando exatamente a meio caminho entre dois números, reduzindo o viés em cálculos ao longo de um grande conjunto de dados e proporcionando um resultado mais estatisticamente significativo. No entanto, isso pode levar a um comportamento inesperado para aqueles não familiarizados com ele, especialmente quando uma precisão integral é esperada em todos os casos.

Em contraste, muitos idiomas de programação e sistemas usam o "arredondamento aritmético" ou "arredondamento para cima", onde um número exatamente a meio caminho entre dois valores possíveis arredondados é sempre arredondado para cima. Ao traduzir ou portar código de outros idiomas para o VBA, os programadores devem ter essas diferenças em mente para evitar erros sutis ou imprecisões em aplicações financeiras e estatísticas.

Embora o VBA ofereça uma variedade de funções para arredondamento, a ausência de funções `Ceiling` e `Floor` (sem recorrer à WorksheetFunction do Excel) destaca uma limitação em suas capacidades nativas. Programadores vindo de idiomas mais ricos em recursos podem achar essas omissões inconvenientes e podem precisar implementar soluções personalizadas ou adaptar seus cálculos para usar as funções disponíveis. Apesar dessas limitações, compreender e usar corretamente as funções de arredondamento do VBA pode ajudar a garantir que os cálculos numéricos sejam tanto precisos quanto atendam aos requisitos da maioria das aplicações.
