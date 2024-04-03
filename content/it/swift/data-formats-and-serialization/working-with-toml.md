---
date: 2024-01-26 04:26:29.948785-07:00
description: "TOML (Tom's Obvious, Minimal Language) \xE8 un formato di serializzazione\
  \ dei dati che \xE8 facile da leggere grazie alla sua chiara semantica. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.793350-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \xE8 un formato di serializzazione\
  \ dei dati che \xE8 facile da leggere grazie alla sua chiara semantica."
title: Lavorare con TOML
weight: 39
---

## Cos'è & Perché?
TOML (Tom's Obvious, Minimal Language) è un formato di serializzazione dei dati che è facile da leggere grazie alla sua chiara semantica. I programmatori utilizzano TOML per file di configurazione dove la leggibilità da parte degli umani e l'elaborazione semplice da parte delle macchine sono fondamentali.

## Come fare:
Per iniziare, hai bisogno di un analizzatore TOML. Swift non ne ha uno incorporato, quindi usiamo `TOMLDecoder`. Installalo tramite Swift Package Manager e poi serializza e deserializza TOML con facilità.

```Swift
import TOMLDecoder

let tomlString = """
title = "Esempio TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Titolo: \(config.title), Proprietario: \(config.owner.name), Nascita: \(config.owner.dob)")
    } catch {
        print("Errore nell'analisi del TOML: \(error)")
    }
}
```

Questo codice produce:
```
Titolo: Esempio TOML, Proprietario: Tom Preston-Werner, Nascita: 1979-05-27 07:32:00 +0000
```

## Approfondimento
TOML è stato progettato da Tom Preston-Werner, co-fondatore di GitHub, come un'alternativa più amichevole per gli umani a formati come JSON o YAML. Punta alla chiarezza, riducendo le possibilità di interpretazione errata da parte di un umano o di una macchina. Per quanto riguarda le alternative, YAML e JSON sono i soliti sospettati, con YAML orientato verso la leggibilità umana e JSON come opzione più semplice amichevole per le macchine. Quando si lavora con TOML in Swift, non disponiamo di un parser nativo. Tuttavia, librerie di terze parti come `TOMLDecoder` facilitano la conversione semplice tra stringhe TOML e tipi Swift, specificamente tramite i protocolli `Codable` introdotti in Swift 4 che hanno semplificato la serializzazione.

## Vedi Anche
- Lo standard TOML: https://toml.io
- GitHub per `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Documentazione Swift su `Codable`: https://developer.apple.com/documentation/swift/codable
- Confronto tra formati di serializzazione dei dati: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
