---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:28.238931-07:00
description: "Arrays associativos, frequentemente conhecidos como dicion\xE1rios no\
  \ Visual Basic para Aplica\xE7\xF5es (VBA), permitem que programadores criem cole\xE7\
  \xF5es de pares\u2026"
lastmod: '2024-02-25T18:49:44.024224-07:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, frequentemente conhecidos como dicion\xE1rios no Visual\
  \ Basic para Aplica\xE7\xF5es (VBA), permitem que programadores criem cole\xE7\xF5\
  es de pares\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O Que & Por Quê?

Arrays associativos, frequentemente conhecidos como dicionários no Visual Basic para Aplicações (VBA), permitem que programadores criem coleções de pares chave-valor. Esta funcionalidade é crucial para o armazenamento e recuperação eficiente de dados, oferecendo uma maneira mais flexível e intuitiva de gerenciar dados do que os índices de arrays tradicionais.

## Como:

No VBA, o objeto `Dictionary` oferece funcionalidade similar a arrays associativos. Primeiro, você deve adicionar uma referência ao Tempo de Execução de Script da Microsoft para usá-lo:

1. No editor do VBA, vá para Ferramentas > Referências...
2. Marque "Microsoft Scripting Runtime" e clique em OK.

Aqui está como declarar, popular e acessar itens em um `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Adicionando itens
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' Acessando itens
Debug.Print sampleDictionary.Item("Name")  ' Saída: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Saída: 29

' Verificando se uma chave existe
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Chave de Occupation Existe"
End If

' Removendo itens
sampleDictionary.Remove("Occupation")

' Percorrendo o dicionário
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Aprofundamento

O objeto `Dictionary` opera por baixo dos panos com componentes do Windows Scripting Host. Assim sendo, é um objeto COM de vínculo tardio, que era uma maneira comum de estender a funcionalidade do VBA no passado. Seu uso no VBA pode aumentar significativamente a capacidade da linguagem de manipular conjuntos de dados complexos sem impor uma estrutura rígida, como visto em arrays tradicionais ou intervalos do Excel.

Uma limitação a ter em mente é que acessar o `Dictionary` requer a configuração de uma referência ao Tempo de Execução de Script da Microsoft, o que pode complicar a distribuição de seus projetos VBA. Alternativas como Collections existem dentro do VBA, mas carecem de algumas das principais funcionalidades do `Dictionary`, como a capacidade de verificar facilmente a existência de uma chave sem disparar um erro.

Em contextos de programação mais recentes, linguagens como Python oferecem suporte integrado para arrays associativos (conhecidos como dicionários em Python também) sem a necessidade de adicionar referências externas. Este suporte integrado simplifica o processo e oferece funcionalidades mais avançadas de forma imediata. No entanto, dentro dos limites do VBA e para aplicações específicas voltadas para a automação de tarefas na suíte Microsoft Office, usar o objeto `Dictionary` continua sendo um método poderoso e relevante para estruturas de dados do tipo array associativo.
