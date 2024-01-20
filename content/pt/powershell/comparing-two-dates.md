---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas é verificar sua equivalência, diferença, ou qual vem antes ou depois. Programadores fazem isso quando precisam trabalhar com logfiles, cronogramas, ferramentas de agendamento ou simplesmente manipulação de datas.

## Como Fazer:

No PowerShell, você pode comparar datas facilmente. Aqui está um exemplo de como fazer isso.

```PowerShell
# Primeiro, criamos duas datas.
$data1 = Get-Date "2023-08-01"
$data2 = Get-Date "2023-08-05"

# Agora, vamos comparar as duas datas.
if ($data1 -gt $data2) {
    "Data1 é maior do que a Data2."
} elseif ($data1 -lt $data2) {
    "Data1 é menor do que a Data2."
} else {
    "Data1 é igual à Data2."
}
```

Na execução acima, a saída será: "Data1 é menor do que a Data2.".

## Aprofundando 

Historicamente, o PowerShell tem facilitado para os programadores lidando com a manipulação de datas e tempo. Seu uso de objetos em vez de strings leva a menos erros e menos código.

Em termos de alternativas, muitas outras linguagens de programação também permitem a comparação de datas, embora a implementação possa variar. C#, Python, e JavaScript são apenas alguns exemplos.

Vale a pena notar que quando você compara datas no PowerShell, o que você realmente está comparando é a quantidade de tempo que passou desde a época do Unix (1 de janeiro de 1970). O PowerShell armazena datas como essa quantidade de tempo em ticks, e compara esses valores para determinar a diferença entre duas datas.

## Veja Também

Se você quiser saber mais sobre a manipulação de datas no PowerShell, confira estes recursos úteis. 
1. Trabalhando com datas e tempo no PowerShell: https://www.red-gate.com/simple-talk/sysadmin/powershell/working-with-dates-and-times-in-powershell/ 
2. PowerShell Date Comparison: https://adamtheautomator.com/powershell-date-comparison/
3. Convertendo e formatando datas no PowerShell: https://www.petri.com/powershell-basics-date-manipulation