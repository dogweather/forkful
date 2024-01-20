---
title:                "Lavorare con yaml"
html_title:           "PowerShell: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con YAML significa gestire dati strutturati in un formato leggibile sia per gli esseri umani che per le macchine. I programmatori utilizzano spesso YAML per definire configurazioni, come ad esempio nel caso di configurazioni per server o applicazioni.

## Come:
Il seguente è un semplice esempio di come creare e leggere un file YAML utilizzando PowerShell:

```PowerShell
# Creazione di un file YAML
$data = @{
    nome = "Mario"
    cognome = "Rossi"
    eta = 35
}

$data | ConvertTo-Yaml | Out-File C:\Users\mario.yaml

# Lettura di un file YAML
$data = Get-Content C:\Users\mario.yaml | ConvertFrom-Yaml
```

L'output del file YAML creato sarà il seguente:

```yaml
nome: Mario
cognome: Rossi
eta: 35
```

Per scrivere dati più complessi, è possibile utilizzare la sintassi di elenco di voci per creare una struttura ad albero:

```PowerShell
$data = @{
    studenti = @(
        @{
            nome = "Maria"
            voti = @(
                @{
                    corso = "Matematica"
                    voto = 9
                },
                @{
                    corso = "Italiano"
                    voto = 8
                }
            )
        },
        @{
            nome = "Luca"
            voti = @(
                @{
                    corso = "Matematica"
                    voto = 7
                },
                @{
                    corso = "Italiano"
                    voto = 9
                }
            )
        }
    )
}

$data | ConvertTo-Yaml | Out-File C:\Users\studenti.yaml
```

Il seguente è l'output del file YAML:

```yaml
studenti:
- nome: Maria
  voti:
    - corso: Matematica
      voto: 9
    - corso: Italiano
      voto: 8
- nome: Luca
  voti:
    - corso: Matematica
      voto: 7
    - corso: Italiano
      voto: 9
```

## Approfondimento:
La storia di YAML risale al 2001, quando è stata proposta come un modo più semplice di scrivere file di configurazione rispetto a XML. Anche se YAML è diventato ampiamente utilizzato nella comunità dei programmatori, esistono anche alternative come JSON e TOML.

Per implementare il supporto YAML in PowerShell, viene utilizzato un modulo chiamato YamlDotNet. Per utilizzarlo, è necessario prima installare il modulo utilizzando il gestore pacchetti NuGet di PowerShell:

```PowerShell
Install-Package YamlDotNet
```

## Vedi anche:
- [Sito ufficiale di YAML](https://yaml.org/)
- [YamlDotNet su GitHub](https://github.com/aaubry/YamlDotNet)