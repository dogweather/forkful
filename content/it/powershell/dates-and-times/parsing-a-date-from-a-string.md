---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:12.787007-07:00
description: "Come fare: PowerShell semplifica l'analisi delle date da stringhe con\
  \ il suo cmdlet `Get-Date` e acceleratore di tipo `[datetime]`, che funzionano bene\u2026"
lastmod: '2024-03-13T22:44:43.651958-06:00'
model: gpt-4-0125-preview
summary: PowerShell semplifica l'analisi delle date da stringhe con il suo cmdlet
  `Get-Date` e acceleratore di tipo `[datetime]`, che funzionano bene per i formati
  di data standard.
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:
PowerShell semplifica l'analisi delle date da stringhe con il suo cmdlet `Get-Date` e acceleratore di tipo `[datetime]`, che funzionano bene per i formati di data standard. Per stringhe di date più complesse o non standard, si può utilizzare il metodo `[datetime]::ParseExact` per specificare il formato esatto.

### Usando `Get-Date` e `[datetime]`:
```powershell
# Conversione semplice usando Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Output dell'esempio:**
```
Sabato, 1 Aprile 2023 00:00:00
```

```powershell
# Usando l'acceleratore di tipo [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Output dell'esempio:**
```
Sabato, 1 Aprile 2023 00:00:00
```

### Usando `[datetime]::ParseExact` per formati non standard:
Per formati non riconosciuti automaticamente, puoi definire il formato esatto per garantire un'analisi corretta.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Output dell'esempio:**
```
Sabato, 1 Aprile 2023 14:00:00
```

### Sfruttando Librerie di Terze Parti
Sebbene PowerShell sia di per sé piuttosto potente per l'analisi delle date, per scenari molto complessi o funzionalità aggiuntive, potresti esplorare librerie .NET come NodaTime, anche se per molti casi d'uso tipici, le capacità native di PowerShell dovrebbero essere sufficienti.

```powershell
# Usando NodaTime solo come illustrazione, nota che devi aggiungere la libreria al tuo progetto
# Install-Package NodaTime -Version 3.0.5
# Usando NodaTime per analizzare una data
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Nota sull'esempio:** Il codice sopra è una illustrazione concettuale. Nella pratica, assicurati che NodaTime sia correttamente aggiunto al tuo progetto affinché i tipi e i metodi siano disponibili.
