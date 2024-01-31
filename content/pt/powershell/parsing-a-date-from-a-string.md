---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:37:56.170495-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Analisar uma data a partir de uma string significa converter o texto que representa uma data para um formato que o PowerShell compreende e pode trabalhar. Programadores fazem isso para manipular datas em scripts, desde calcular diferenças de tempo até agendar tarefas.

## Como Fazer:
```PowerShell
# Analisando uma data de uma string usando 'Get-Date'
$dataString = "25/04/2023 14:00"
$dataObjeto = Get-Date $dataString

# Exibindo o objeto data
$dataObjeto

# Formatação personalizada de data/hora
$dataFormatada = $dataObjeto.ToString("dd/MM/yyyy HH:mm")
Write-Output $dataFormatada
```
Saída da amostra:
```
terça-feira, 25 de abril de 2023 14:00:00
25/04/2023 14:00
```

## Mergulho Profundo
Antigamente, analisar datas no PowerShell podia ser confuso devido a diferentes formatos e locais. Com o cmdlet `Get-Date`, isso se simplificou, mas ainda precisamos ter atenção ao formato de data do sistema ou especificar o formato desejado. Existem alternativas como `[datetime]::ParseExact()`, que dão mais controle sobre o formato da data:

```PowerShell
$dataString = "25/04/2023 14:00"
$formato = "dd/MM/yyyy HH:mm"
$provider = [System.Globalization.CultureInfo]::InvariantCulture
$dataObjeto = [datetime]::ParseExact($dataString, $formato, $provider)
Write-Output $dataObjeto
```

Implementar a análise de datas pode variar de acordo com o idioma e a localização. O PowerShell usa as configurações de cultura do sistema, então está pronto para lidar com formatos tipicamente brasileiros.

## Veja Também
- [Get-Date](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
- [Personalizando formatos de data e hora](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Classe CultureInfo](https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- [Blog do PowerShell Scripting](https://devblogs.microsoft.com/scripting/)
