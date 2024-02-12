---
title:                "Trabalhando com JSON"
date:                  2024-02-01T22:05:55.806538-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

JSON (JavaScript Object Notation) é um formato leve de intercâmbio de dados que é fácil de ler e escrever por humanos, e de analisar e gerar por máquinas. Os programadores usam o JSON para transmitir dados entre um servidor e uma aplicação web ou para armazenar informações de maneira estruturada e acessível em uma variedade de ambientes de programação, incluindo o Visual Basic for Applications (VBA).

## Como Fazer:

O VBA não oferece suporte nativo para a análise ou geração de JSON, então usaremos uma linguagem de script como o JScript (via o objeto ScriptControl) para analisar cadeias de caracteres JSON e construir objetos JSON. Veja como você pode analisar uma cadeia de caracteres JSON no VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Nome: " & parsed.name & ", Idade: " & parsed.age & ", Cidade: " & parsed.city
End Sub
```

Para gerar JSON, você poderia usar uma abordagem semelhante, construindo a cadeia de caracteres JSON por concatenação:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Aprofundamento

As abordagens mostradas aproveitam o ScriptControl para lidar com JSON, essencialmente externalizando o trabalho para um motor de JavaScript. Esta é uma solução criativa, mas não necessariamente a maneira mais eficiente ou moderna de trabalhar com JSON em um contexto VBA. Em aplicações mais complexas, este método pode se tornar pesado e introduzir sobrecarga de desempenho ou preocupações de segurança, já que o ScriptControl executa em um ambiente que tem acesso total ao computador host.

Outros ambientes de programação, como Python ou JavaScript, oferecem suporte integrado para JSON, tornando-os mais adequados para aplicações que exigem manipulação extensiva de JSON. Essas línguas fornecem bibliotecas abrangentes que facilitam não apenas a análise e geração, mas também a consulta e formatação de dados JSON.

Apesar dessas limitações no VBA, entender como trabalhar com JSON é vital em um mundo onde a troca de dados baseada na web e arquivos de configuração são predominantemente formatados em JSON. Para programadores de VBA, dominar essas técnicas abre oportunidades para integrar com APIs da web, interpretar arquivos de configuração ou até mesmo construir aplicações web simples. No entanto, quando os projetos crescem em complexidade ou exigem alta performance, os desenvolvedores podem considerar explorar ambientes de programação mais amigáveis ao JSON.
