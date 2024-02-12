---
title:                "Refatoração"
aliases:
- /pt/vba/refactoring/
date:                  2024-02-01T21:59:52.186511-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Refatoração em programação envolve modificar a estrutura do código sem alterar seu comportamento, para melhorar aspectos como legibilidade, manutenibilidade ou desempenho. Programadores refatoram para tornar o código mais eficiente, fácil de entender, fácil de modificar no futuro e para reduzir a probabilidade de bugs.

## Como fazer:

Considere um exemplo básico em Visual Basic for Applications (VBA) onde temos uma sub-rotina que imprime detalhes de um funcionário. Inicialmente, o código está confuso, difícil de manter ou estender.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "TI"
    
    MsgBox "Nome: " & name & vbCrLf & "Idade: " & age & vbCrLf & "Departamento: " & department
End Sub
```

Passo de refatoração 1: Extrair método. Uma das técnicas de refatoração mais comuns é pegar um pedaço específico do código e mover para o seu próprio método. Isso torna o código mais modular e fácil de entender.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "TI"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Nome: " & name & vbCrLf & "Idade: " & age & vbCrLf & "Departamento: " & department
End Sub
```

Passo de refatoração 2: Usar uma estrutura. Este passo envolve usar uma estrutura de dados para armazenar dados relacionados, melhorando a clareza do código e tornando mais fácil passar dados agrupados.

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "TI"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Nome: " & emp.name & vbCrLf & "Idade: " & emp.age & vbCrLf & "Departamento: " & emp.department
End Sub
```

Esses passos transformam o código confuso em um código modular, estruturado, melhorando significativamente a legibilidade e a manutenibilidade.

## Aprofundamento

O conceito de refatoração é tão antigo quanto a própria programação, mas foi o livro de Martin Fowler, "Refactoring: Improving the Design of Existing Code", que o trouxe para o mainstream, enfatizando sua importância no processo de desenvolvimento de software. Em Visual Basic for Applications, a refatoração pode ser um pouco mais desafiadora devido à falta de ferramentas integradas encontradas em ambientes de desenvolvimento integrados (IDEs) mais modernos que suportam refatoração automatizada.

No entanto, isso não diminui sua importância. Mesmo no VBA, a aplicação manual de técnicas básicas de refatoração pode melhorar bastante a base de código, tornando-a mais limpa e eficiente. Embora o VBA não tenha as mesmas conveniências modernas, os princípios de um bom design de código permanecem universais. Desenvolvedores vindos de outras linguagens podem achar o processo manual tedioso, mas sem dúvida apreciarão os benefícios de investir tempo na melhoria da qualidade do código desde o início.

Para ambientes de desenvolvimento mais robustos ou ao trabalhar em projetos particularmente sofisticados, pode valer a pena explorar alternativas que ofereçam ferramentas de refatoração mais poderosas ou converter projetos VBA para uma linguagem .NET onde o Visual Studio fornece suporte extensivo à refatoração. No entanto, entender e aplicar princípios de refatoração no VBA é uma habilidade valiosa que sublinha a importância de escrever um código limpo e manutenível, independentemente do ambiente.
