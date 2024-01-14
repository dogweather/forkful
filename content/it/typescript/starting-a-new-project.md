---
title:    "TypeScript: Iniziare un nuovo progetto"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

In questa era digitale in continua evoluzione, è sempre più importante per gli sviluppatori saper utilizzare strumenti moderni e efficienti per creare applicazioni web e mobile di alta qualità. TypeScript è uno di questi strumenti che offre diversi vantaggi rispetto ad altri linguaggi di programmazione. In questo post, ti guiderò attraverso i passaggi necessari per iniziare un nuovo progetto TypeScript.

## Come

Per iniziare, assicurati di avere il compilatore TypeScript installato sul tuo computer. Puoi farlo facilmente con il gestore di pacchetti NPM. Apri il tuo terminale e digita il seguente comando:

```
npm install -g typescript
```

Una volta installato, accertati di avere una cartella dedicata al tuo progetto ed entra all'interno del terminale al suo interno. Ora puoi creare un nuovo file di configurazione TypeScript "tsconfig.json" con il comando:

```
tsc --init
```

Questo file configurerà il tuo progetto per l'utilizzo di TypeScript, inclusi i percorsi, le opzioni di compilazione e i file di output.

Successivamente, crea il tuo primo file TypeScript con l'estensione ".ts" all'interno della tua cartella di progetto. Ora puoi utilizzare molte delle funzionalità avanzate di TypeScript, come la tipizzazione dei dati e le classi. Ad esempio:

```
TypeScriptclass User {
  name: string;
  age: number;

  constructor(name: string, age: number) {
    this.name = name;
    this.age = age;
  }

  greet() {
    console.log("Ciao, sono " + this.age + "anni e il mio nome è " + this.name );
  }
}

let user = new User("Mario", 25);

user.greet();
```

Il codice sopra definisce una classe "User" con due attributi, "name" di tipo stringa e "age" di tipo numerico. Il costruttore della classe viene utilizzato per inizializzare questi attributi quando viene creata un'istanza della classe. La funzione "greet" utilizza entrambi gli attributi per stampare un saluto personalizzato. Al momento dell'esecuzione, il seguente output verrà prodotto:

```
Ciao, sono 25 anni e il mio nome è Mario
```

È possibile utilizzare TypeScript con qualsiasi framework JavaScript, come Angular, React o Vue.js per creare applicazioni web o mobile ancora più potenti.

## Deep Dive

Un vantaggio aggiuntivo di TypeScript è la possibilità di utilizzare i tipi di dati per aiutare a prevenire errori di codifica. Quando si utilizza un'IDE compatibile con TypeScript, come VS Code, si ottengono suggerimenti sugli errori di sintassi e sui possibili problemi di tipo mentre si digita il codice. Inoltre, il compilatore TypeScript fornisce un output più esplicativo degli errori rispetto ai linguaggi di programmazione tradizionali. Ciò consente di risolvere gli errori in modo più efficiente e di mantenere il codice più affidabile.

Inoltre, il supporto per le annotazioni dei tipi consente a nuovi sviluppatori di apprendere più rapidamente e comprendere meglio il codice. Inoltre, TypeScript è un linguaggio di scripting flessibile che offre molte opzioni di personalizzazione e integrazione con altri strumenti come React Native per lo sviluppo di app mobili o NestJS per la creazione di API REST.

## Vedi anche

- [Sito ufficiale di TypeScript](https://www.typescriptlang.org/)
- [Documentazione di TypeScript](https://www.typescriptlang.org/docs/home.html)