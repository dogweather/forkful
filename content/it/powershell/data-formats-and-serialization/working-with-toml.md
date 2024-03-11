---
date: 2024-01-26 04:24:58.369723-07:00
description: "TOML, acronimo di Tom's Obvious, Minimal Language, \xE8 un formato di\
  \ serializzazione dei dati che \xE8 facile da leggere grazie alla sua chiara semantica.\
  \ I\u2026"
lastmod: '2024-03-11T00:14:17.283373-06:00'
model: gpt-4-0125-preview
summary: "TOML, acronimo di Tom's Obvious, Minimal Language, \xE8 un formato di serializzazione\
  \ dei dati che \xE8 facile da leggere grazie alla sua chiara semantica. I\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cos'è e Perché?

TOML, acronimo di Tom's Obvious, Minimal Language, è un formato di serializzazione dei dati che è facile da leggere grazie alla sua chiara semantica. I programmatori lo utilizzano per i file di configurazione, poiché offre un equilibrio tra leggibilità umana e compatibilità con le macchine.

## Come fare:

In PowerShell, non esiste un cmdlet nativo per analizzare TOML. Tipicamente, si utilizzerebbe un modulo o si convertirebbe TOML in JSON con uno strumento come `toml-to-json` se si desidera lavorare con PowerShell. Ecco come si farebbe con un modulo fittizio `PowerShellTOML`:

```PowerShell
# Prima, installa il modulo (immaginario, per dimostrazione)
Install-Module PowerShellTOML

# Importa un file TOML
$config = Import-TomlConfig -Path './config.toml'

# Accedere a un valore
Write-Output $config.database.server

# Contenuto TOML di esempio in 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Output di esempio:
# 192.168.1.1
```

## Approfondimento

TOML è stato creato da Tom Preston-Werner, co-fondatore di GitHub, come alternativa più semplice a XML e YAML per i file di configurazione. La sua prima versione è apparsa nel 2013. TOML è paragonabile a JSON ma è progettato per essere più amichevole per gli umani, rendendolo una buona scelta per configurazioni mantenute da persone. Alternative includono YAML, JSON e XML.

In termini di implementazione, un modulo PowerShell per TOML sarebbe tipicamente un wrapper attorno a una libreria TOML scritta in un linguaggio più orientato alle prestazioni come C#. PowerShell non ha supporto integrato per TOML, motivo per cui è necessario tale modulo per interfacciarsi comodamente con il formato TOML.

## Vedi Anche

- Standard TOML: https://toml.io/en/
- Repository GitHub per il modulo `toml` di PowerShell (se esiste al momento della lettura): https://github.com/powershell/PowerShellTOML
- Un'introduzione a TOML: https://github.com/toml-lang/toml
- Confronto dei formati di serializzazione dei dati: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
