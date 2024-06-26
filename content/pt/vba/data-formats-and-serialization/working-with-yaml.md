---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:23.310552-07:00
description: "Como Fazer: Trabalhar com YAML em VBA requer o entendimento de como\
  \ analisar e converter YAML para um formato que o VBA possa manipular facilmente,\u2026"
lastmod: '2024-03-13T22:44:46.434420-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com YAML em VBA requer o entendimento de como analisar e converter\
  \ YAML para um formato que o VBA possa manipular facilmente, normalmente dicion\xE1\
  rios ou cole\xE7\xF5es."
title: Trabalhando com YAML
weight: 41
---

## Como Fazer:
Trabalhar com YAML em VBA requer o entendimento de como analisar e converter YAML para um formato que o VBA possa manipular facilmente, normalmente dicionários ou coleções. Infelizmente, o VBA não suporta nativamente a análise ou serialização de YAML. No entanto, você pode usar uma combinação de ferramentas de conversão JSON e objetos de dicionário para trabalhar com dados YAML, considerando a proximidade do YAML com JSON.

Primeiro, converta seus dados YAML para JSON usando um conversor online ou uma ferramenta de conversão YAML para JSON dentro do seu ambiente de desenvolvimento. Uma vez convertido, você pode usar o seguinte exemplo para analisar JSON em VBA, notando que esta abordagem permite, indiretamente, trabalhar com YAML:

```vb
' Adicionar referência ao Microsoft Scripting Runtime para Dictionary
' Adicionar referência ao Microsoft XML, v6.0 para análise de JSON

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' Isso é JSON convertido de YAML
    
    ' Assumindo que você tenha uma função de análise de JSON
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Name: " & parsedData("name")
    Debug.Print "Age: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Espaço reservado para lógica de análise de JSON - você pode usar uma biblioteca externa aqui
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
Neste exemplo, a função `JsonParser` é um substituto para onde você faria a análise do JSON. Diversas bibliotecas estão disponíveis para ajudar com a análise de JSON, já que bibliotecas de análise direta de YAML para VBA são escassas.

## Aprofundamento
A ausência de manipulação direta de YAML no VBA pode ser atribuída à sua idade e ao ambiente para o qual foi construído, que inicialmente não foi projetado com formatos modernos de serialização de dados em mente. O YAML, em si, emergiu como um formato popular de configuração e serialização no início dos anos 2000, coincidindo com o advento de aplicações que requerem arquivos de configuração mais amigáveis ao humano.

Programadores tipicamente recorrem a ferramentas ou bibliotecas externas para preencher a lacuna entre VBA e YAML. Isso geralmente envolve converter YAML para JSON, como mostrado, devido ao suporte JSON disponível através de várias bibliotecas e a semelhança entre JSON e YAML em termos de estrutura e propósito.

Embora trabalhar diretamente com YAML em VBA demonstre a flexibilidade da linguagem, vale notar que outros ambientes de programação (por exemplo, Python ou JavaScript) fornecem suporte mais nativo e sem interrupções para YAML. Essas alternativas podem ser mais adequadas para projetos fortemente dependentes de YAML para configuração ou serialização de dados. No entanto, para aqueles comprometidos ou que necessitam de VBA, o método indireto através da conversão JSON permanece uma abordagem viável e útil para gerenciar e manipular dados YAML.
