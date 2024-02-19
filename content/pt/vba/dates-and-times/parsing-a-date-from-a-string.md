---
aliases:
- /pt/vba/parsing-a-date-from-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:24.850040-07:00
description: "Analisar uma data a partir de uma string em Visual Basic for Applications\
  \ (VBA) \xE9 sobre converter texto que representa uma data em um tipo de dados de\u2026"
lastmod: 2024-02-18 23:08:57.984736
model: gpt-4-0125-preview
summary: "Analisar uma data a partir de uma string em Visual Basic for Applications\
  \ (VBA) \xE9 sobre converter texto que representa uma data em um tipo de dados de\u2026"
title: Analisando uma data a partir de uma string
---

{{< edit_this_page >}}

## O Que & Por Que?

Analisar uma data a partir de uma string em Visual Basic for Applications (VBA) é sobre converter texto que representa uma data em um tipo de dados de data. Os programadores fazem isso para manipular datas de forma mais eficaz em suas aplicações, como para comparações, cálculos ou propósitos de formatação.

## Como Fazer:

VBA oferece uma maneira direta de analisar uma string em data usando a função `CDate` ou a função `DateValue`. No entanto, é crucial que a string esteja em um formato de data reconhecível.

Aqui está um exemplo básico usando `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date

    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Data Analisada: "; parsedDate
End Sub
```

Se você executar este código, a saída na Janela Imediata (acessível via `Ctrl+G` no editor VBA) seria:

```
Data Analisada: 1/4/2023 
```

Alternativamente, você pode usar a função `DateValue`, que é mais específica para datas (ignorando a parte do tempo):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "1 de abril de 2023"
    parsedDate = DateValue(dateString)

    Debug.Print "Data Analisada usando DateValue: "; parsedDate
End Sub
```

A saída de exemplo para isso mostraria de forma semelhante na Janela Imediata:

```
Data Analisada usando DateValue: 1/4/2023
```

Tenha em mente que o sucesso da análise depende do formato de data da string corresponder às configurações do sistema ou aplicativo.

## Aprofundamento

Internamente, quando VBA analisa uma string para uma data, ele usa as configurações regionais do sistema operacional Windows para interpretar o formato da data. Isso é crucial para entender porque uma string de data que analisa perfeitamente em um sistema pode causar um erro em outro se eles usarem configurações de data/hora diferentes.

Historicamente, o manuseio de datas tem sido uma fonte comum de bugs em aplicações, particularmente aquelas que são usadas internacionalmente. Essa dependência das configurações regionais no VBA é o motivo pelo qual alguns podem considerar alternativas como o formato ISO 8601 (por exemplo, "AAAA-MM-DD") para representação e análise de data inambígua em diferentes sistemas. Infelizmente, o VBA não oferece suporte nativo ao ISO 8601, e uma análise manual seria necessária para conformidade estrita.

Para análise de datas complexas além do que `CDate` ou `DateValue` podem lidar, ou para garantir análise consistente independentemente das configurações de localidade do sistema, os programadores podem recorrer a funções de análise personalizadas. Isso poderia envolver dividir a string de data em componentes (ano, mês, dia) e construir uma data usando a função `DateSerial`. Outros podem escolher linguagens ou bibliotecas mais poderosas projetadas com a internacionalização em mente para tais tarefas.
