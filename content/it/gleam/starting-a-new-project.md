---
title:    "Gleam: Iniziare un nuovo progetto"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

### Perché

Se stai leggendo questo post, probabilmente vuoi iniziare un nuovo progetto in Gleam ma ti chiedi perché dovresti farlo. Bene, Gleam è un linguaggio di programmazione funzionale che offre grande sicurezza e affidabilità. Inoltre, ha una sintassi facile da imparare e un'ottima community di supporto. Iniziare un progetto in Gleam può portare a grandi risultati e miglioramenti nella gestione del tuo codice.

### Come Fare

Per iniziare un nuovo progetto in Gleam, è necessario avere sul tuo computer il compilatore Gleam e il gestore dei pacchetti. Una volta installati, puoi seguire questi semplici passaggi.

```Gleam
gleam new my_project
```

Questo creerà una nuova cartella chiamata "my_project" che conterrà una struttura di base per il tuo progetto, tra cui il file "gleam.toml" per la configurazione dei pacchetti e il file "src/my_project.gleam" con un semplice esempio di codice.

```Gleam
pub fn greet(name) {
  "Ciao, " ++ name ++ "!"  
}
```

Una volta che hai scritto il tuo codice nel file "src/my_project.gleam", puoi compilare il tuo progetto eseguendo il comando:

```Gleam
gleam build
```

Questo compilerà il tuo codice e genererà un eseguibile all'interno della cartella "bin". Per eseguire il tuo programma, basta digitare:

```Gleam
./bin/my_project
```

### Approfondimento

Se desideri saperne di più su come avviare un nuovo progetto in Gleam, puoi consultare la documentazione ufficiale. Lì troverai maggiori dettagli sulla configurazione dei pacchetti, sulla struttura dei progetti e su come compilare e eseguire il codice.

### Vedi Anche

- Documentazione di Gleam: https://gleam.run/book/
- Comunità di Gleam su Discord: https://discord.gg/gleam-lang
- Esempi di progetti in Gleam: https://github.com/gleam-lang/awesome-gleam