---
title:                "Trabalhando com números complexos"
aliases:
- /pt/vba/working-with-complex-numbers.md
date:                  2024-02-01T22:07:40.976281-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com números complexos envolve realizar operações matemáticas em números que possuem tanto uma parte real quanto uma parte imaginária. Programadores frequentemente lidam com números complexos em domínios como engenharia, física e qualquer área que envolva resolver equações que não são possíveis apenas com números reais.

## Como fazer:

Em Visual Basic for Applications (VBA), lidar com números complexos pode ser um tanto quanto menos direto comparado com linguagens que possuem suporte nativo para eles. No entanto, você pode gerenciar operações complexas criando funções ou utilizando funções de bibliotecas existentes. Vamos explorar um exemplo básico de adição, subtração, multiplicação e divisão de números complexos:

```vb
' Função para adicionar números complexos
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Extraindo partes reais e imaginárias dos números complexos
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Realizando a adição
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Exemplo de uso
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "Resultado da Adição: " & result  ' Saída: Resultado da Adição: 4+9i
End Sub
```

Embora isso demonstre a adição, abordagens similares podem ser adaptadas para subtração, multiplicação e divisão. Para operações complexas além da aritmética básica, pode ser interessante explorar bibliotecas externas ou integrar outras soluções que suportem operações com números complexos de forma mais nativa.

## Aprofundamento:

O VBA não inclui suporte embutido para números complexos, um aspecto no qual fica atrás de linguagens como Python, que tem uma classe de números complexos (`complex`) ou C++ com sua Standard Template Library (`std::complex`). Historicamente, a necessidade de manipular números complexos diretamente no VBA é relativamente rara, já que é frequentemente utilizado para automação, manipulando aplicações do Office e tarefas que tradicionalmente não requerem cálculos matemáticos complexos. Quando o VBA foi concebido e desenvolvido, seus casos de uso eram principalmente focados em aplicações comerciais em vez de computação científica, o que pode explicar a omissão.

Para tarefas que requerem manipulações extensivas de números complexos, programadores podem achar benéfico usar uma linguagem mais orientada para matemática. No entanto, para aqueles comprometidos ou restritos pelo uso do VBA, escrever funções personalizadas (como ilustrado) ou integrar com softwares que possuem essas capacidades (como MATLAB ou o próprio Excel até certo ponto) são caminhos viáveis a seguir. Apesar de suas limitações, soluções criativas e integrações externas podem estender a utilidade do VBA para domínios aos quais não foi originalmente projetado, incluindo o trabalho com números complexos.
