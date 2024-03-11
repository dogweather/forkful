---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:32.279858-07:00
description: "TOML, che sta per Tom's Obvious, Minimal Language, \xE8 un formato di\
  \ serializzazione dei dati prevalentemente utilizzato per i file di configurazione.\
  \ I\u2026"
lastmod: '2024-03-11T00:14:16.857539-06:00'
model: gpt-4-0125-preview
summary: "TOML, che sta per Tom's Obvious, Minimal Language, \xE8 un formato di serializzazione\
  \ dei dati prevalentemente utilizzato per i file di configurazione. I\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cosa e Perché?

TOML, che sta per Tom's Obvious, Minimal Language, è un formato di serializzazione dei dati prevalentemente utilizzato per i file di configurazione. I programmatori utilizzano TOML per la sua leggibilità e facile mappatura a strutture di dati, permettendo una configurazione semplice delle applicazioni in vari ambienti di programmazione, inclusa Visual Basic for Applications (VBA).

## Come fare:

Lavorare con TOML in VBA comporta l'analisi del file TOML per leggere le configurazioni o impostazioni nel vostro progetto VBA. VBA non ha un supporto integrato per TOML, quindi tipicamente si utilizzerebbe un parser o si convertirebbero i dati TOML in un formato con cui VBA può lavorare facilmente, come JSON o XML. Ecco come eseguire manualmente l'analisi di un semplice file di configurazione TOML:

1. **File di Esempio TOML** (`config.toml`):
```
title = "Esempio TOML"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **Codice VBA per Analizzare TOML**:

Assumendo che il contenuto TOML sia letto in una variabile stringa `tomlStr`, il seguente codice VBA dimostra un approccio semplicistico per analizzare la sezione `[database]`:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'Esempio per accedere ai dati analizzati
    Debug.Print "Server Database: "; config("database")("server")
End Function
```

3. **Output di Esempio** (Finestra Immediata):
```
Server Database: 192.168.1.1
```

## Approfondimento

L'accettazione pratica di TOML nella comunità degli sviluppatori mostra una tendenza verso file di configurazione più semplici e leggibili dall'uomo, in contrasto con l'allora prevalente XML. La filosofia di design di TOML enfatizza semantica chiara e mira ad una facile analisi con un overhead minimo. In VBA, gestire direttamente TOML comporta un'analisi manuale o sfruttare strumenti esterni per convertire TOML in un formato più amichevole per VBA a causa della mancanza di supporto nativo. Sebbene questo metodo di analisi manuale mostri un approccio fondamentale, l'utilizzo di librerie esterne o formati intermedi come JSON può offrire strategie di analisi più robuste e resistenti agli errori. Dato l'ampia integrazione di VBA con Microsoft Office, convertire TOML in JSON e utilizzare le capacità native di analisi JSON di VBA (dove applicabile) o parser JSON di terze parti potrebbe fornire un flusso di lavoro più semplificato. Inoltre, con la continua evoluzione dei formati di serializzazione dei dati, i programmatori dovrebbero considerare anche YAML, che, come TOML, enfatizza la leggibilità umana ma offre diversi compromessi in termini di complessità e flessibilità.
