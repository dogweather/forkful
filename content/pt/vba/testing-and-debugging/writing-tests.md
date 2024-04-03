---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:32.947126-07:00
description: "Como Fazer: Embora o Visual Basic for Applications (VBA) n\xE3o venha\
  \ com uma estrutura de teste integrada semelhante \xE0s dispon\xEDveis em linguagens\
  \ como\u2026"
lastmod: '2024-03-13T22:44:46.416297-06:00'
model: gpt-4-0125-preview
summary: "Embora o Visual Basic for Applications (VBA) n\xE3o venha com uma estrutura\
  \ de teste integrada semelhante \xE0s dispon\xEDveis em linguagens como Python ou\
  \ JavaScript, ainda \xE9 poss\xEDvel implementar procedimentos de teste simples\
  \ para verificar a integridade do seu c\xF3digo."
title: Escrevendo testes
weight: 36
---

## Como Fazer:
Embora o Visual Basic for Applications (VBA) não venha com uma estrutura de teste integrada semelhante às disponíveis em linguagens como Python ou JavaScript, ainda é possível implementar procedimentos de teste simples para verificar a integridade do seu código. Aqui está um exemplo para ilustrar:

Suponha que você tenha uma função em VBA que adiciona dois números:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

Para testar essa função, você pode escrever outro procedimento que valide sua saída contra os resultados esperados:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Teste Passou!", vbInformation
    Else
        MsgBox "Teste Falhou. Esperava 15 mas obteve " & result, vbCritical
    End If
End Sub
```

Executar `TestAddNumbers` exibirá uma caixa de mensagem indicando se o teste passou ou falhou com base na saída da função. Embora este seja um cenário simplificado, você pode construir testes mais complexos incorporando laços, diferentes valores de entrada e testando múltiplas funções.

## Aprofundamento
A abordagem para escrever testes em VBA mostrada aqui é manual e carece dos recursos de estruturas de teste mais sofisticadas disponíveis em outros ambientes de programação, tais como execuções de teste automatizadas, procedimentos de configuração/encerramento e relatórios integrados dos resultados dos testes. Antes da adoção mais ampla de estruturas de teste de unidade e desenvolvimento orientado por testes (TDD), procedimentos de teste manuais similares ao descrito eram comuns. Embora este método seja simples e possa ser eficaz para projetos pequenos ou para fins de aprendizagem, ele não é escalável ou eficiente para projetos maiores ou equipes.

Em ambientes que suportam conjuntos de ferramentas de desenvolvimento mais ricos, os programadores frequentemente recorrem a estruturas como NUnit para aplicações .NET ou JUnit para aplicações Java, que fornecem ferramentas abrangentes para escrever e executar testes sistematicamente. Essas estruturas oferecem recursos avançados, como afirmação de resultados de testes, configuração de objetos mock e medição da cobertura de código.

Para desenvolvedores VBA que procuram capacidades de teste mais avançadas, a alternativa mais próxima pode ser o uso de ferramentas externas ou a integração com outros ambientes de programação. Alguns desenvolvedores usam VBA em conjunto com o Excel para registrar cenários de teste e resultados manualmente. Embora não seja tão conveniente ou automatizado quanto usar uma estrutura de teste dedicada, esses métodos podem ajudar a preencher parcialmente a lacuna, mantendo a confiabilidade das soluções VBA em aplicações complexas ou críticas.
