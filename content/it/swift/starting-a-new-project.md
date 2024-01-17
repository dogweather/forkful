---
title:                "Avviare un nuovo progetto"
html_title:           "Swift: Avviare un nuovo progetto"
simple_title:         "Avviare un nuovo progetto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

Cos'è e Perché?

Iniziamo un nuovo progetto? È semplice: quando i programmatori vogliono creare qualcosa di nuovo, iniziano un nuovo progetto. 
A volte è perché hanno un'idea entusiasmante o un problema che vogliono risolvere. Altre volte, è perché il progetto attuale ha raggiunto un punto di arrivo e vogliono iniziare qualcosa di nuovo.

Come Faccio:

Per avviare un nuovo progetto in Swift, seguire questi semplici passaggi:

1. Aprire Xcode e selezionare "Create a new Xcode project".
2. Selezionare il template "Single View App" e fare clic su "Next".
3. Assegnare un nome al progetto e scegliere un'organizzazione di sviluppo. Fare clic su "Next".
4. Scegliere la posizione in cui salvare il progetto e premere "Create".

Per testare il progetto, è possibile aggiungere del codice all'interno della funzione `viewDidLoad()` del file ViewController.swift. Ad esempio:

```Swift 
print("Ciao, mondo!")
```
L'output sarà "Ciao, mondo!" nella console.

Deep Dive:

In passato, la creazione di un nuovo progetto richiedeva molto più lavoro. Bisognava iniziare da zero e costruire tutto il necessario per far funzionare il progetto. Con l'avvento dei moderni strumenti di sviluppo come Xcode, creare un nuovo progetto è diventato molto più semplice e veloce.

Un'alternativa a Xcode per iniziare un nuovo progetto in Swift è utilizzare Swift Package Manager, un gestore di pacchetti integrato in Swift. Inoltre, a volte i programmatori iniziano un nuovo progetto basandosi su un progetto già esistente, chiamato "forking".

See Also:

- Documentazione ufficiale di Xcode: https://developer.apple.com/xcode/
- Documentazione ufficiale di Swift Package Manager: https://swift.org/package-manager/
- Informazioni su "forking": https://help.github.com/en/github/getting-started-with-github/fork-a-repo