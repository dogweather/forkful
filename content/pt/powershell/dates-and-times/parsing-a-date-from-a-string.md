---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:01.704143-07:00
description: "Como Fazer: O PowerShell torna a an\xE1lise de datas a partir de strings\
  \ direta com seu cmdlet `Get-Date` e o acelerador de tipo `[datetime]`, que funcionam\u2026"
lastmod: '2024-03-13T22:44:46.806270-06:00'
model: gpt-4-0125-preview
summary: "O PowerShell torna a an\xE1lise de datas a partir de strings direta com\
  \ seu cmdlet `Get-Date` e o acelerador de tipo `[datetime]`, que funcionam bem para\
  \ formatos de data padr\xE3o."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como Fazer:
O PowerShell torna a análise de datas a partir de strings direta com seu cmdlet `Get-Date` e o acelerador de tipo `[datetime]`, que funcionam bem para formatos de data padrão. Para strings de data mais complexas ou não padronizadas, o método `[datetime]::ParseExact` pode ser utilizado para especificar o formato exato.

### Usando `Get-Date` e `[datetime]`:
```powershell
# Conversão simples usando Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Saída Exemplo:**
```
Sábado, 1 de Abril de 2023 00:00:00
```

```powershell
# Usando o acelerador de tipo [datetime]
$stringDate = "1 de Abril de 2023"
$date = [datetime]$stringDate
echo $date
```
**Saída Exemplo:**
```
Sábado, 1 de Abril de 2023 00:00:00
```

### Usando `[datetime]::ParseExact` para formatos não padrão:
Para formatos não automaticamente reconhecidos, você pode definir o formato exato para garantir a análise correta.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Saída Exemplo:**
```
Sábado, 1 de Abril de 2023 14:00:00
```

### Aproveitando Bibliotecas de Terceiros
Embora o PowerShell por si só seja bastante poderoso para a análise de datas, para cenários muito complexos ou funcionalidades adicionais, você pode explorar bibliotecas .NET como NodaTime, embora para muitos casos de uso típicos, as capacidades nativas do PowerShell serão suficientes.

```powershell
# Usando NodaTime apenas como uma ilustração, note que você precisa adicionar a biblioteca ao seu projeto
# Install-Package NodaTime -Version 3.0.5
# Usando NodaTime para analisar uma data
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Nota Exemplo:** O código acima é uma ilustração conceitual. Na prática, assegure-se de que o NodaTime esteja corretamente adicionado ao seu projeto para que os tipos e métodos estejam disponíveis.
