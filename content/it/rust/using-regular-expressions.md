---
title:    "Rust: Utilizzando le espressioni regolari"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore, sicuramente hai sentito parlare delle espressioni regolari (o regex). Ma perché dovresti impegnarti a imparare e utilizzare questa funzionalità nel tuo codice Rust?

Le espressioni regolari sono estremamente utili per trovare, sostituire o verificare la presenza di un determinato pattern all'interno di una stringa. Sono uno strumento potente per manipolare i dati e risparmiare tempo e sforzo nel processo.

## Come fare

Per utilizzare le espressioni regolari in Rust, devi prima importare il modulo `regex` nel tuo codice.

```Rust
use regex::Regex;
```

Una volta importato, puoi creare un nuovo oggetto `Regex` passando come argomento il pattern che desideri cercare.

```Rust
let regex_pattern = Regex::new(r"Hello");
```

Per eseguire una ricerca all'interno di una stringa, puoi utilizzare il metodo `find`.

```Rust
let result = regex_pattern.find("Hello world");
```

Il risultato sarà un `Option<Match>` che indica la prima corrispondenza trovata o `None` se non viene trovata alcuna corrispondenza.

Puoi anche utilizzare i metodi `replace` e `replace_all` per sostituire le corrispondenze trovate con un altro testo.

Inoltre, puoi utilizzare le espressioni regolari per estrarre parti specifiche di una stringa utilizzando i "captures".

```Rust
let capture = Regex::new(r"(\d{2})/(\d{2})/(\d{4})")
    .unwrap()
    .captures("31/12/2020");

println!("Day: {}, Month: {}, Year: {}",
    capture.get(1).unwrap().as_str(),
    capture.get(2).unwrap().as_str(),
    capture.get(3).unwrap().as_str());
```

## Approfondimento

Le espressioni regolari possono sembrare complicate e intimidatorie, ma una volta che hai capito come funzionano, possono risultare estremamente utili e risparmiare molto tempo nella tua programmazione.

Una cosa importante da tenere a mente è che le espressioni regolari utilizzate in Rust seguono la sintassi PCRE (Perl Compatible Regular Expressions), quindi se hai già familiarità con quest'ultima, sarai a tuo agio nell'utilizzo delle regex in Rust.

Inoltre, le espressioni regolari in Rust sono totalmente sicure da usare e non dovrai preoccuparti di problemi di sicurezza che potrebbero sorgere in altri linguaggi.

## Vedi anche

- Documentazione ufficiale delle espressioni regolari in Rust: https://doc.rust-lang.org/regex/regex/index.html
- Tutorial completo sulle espressioni regolari in Rust: https://www.banyansecurity.io/2020/05/31/regular-expressions-in-rust/