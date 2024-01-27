---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Le espressioni regolari, o regex, sono pattern che aiutano a identificare sequenze di caratteri all'interno di stringhe di testo con grande flessibilità, adattandosi ai vari casi con precisione. I programmatori le usano per effettuare ricerche complesse, convalidare dati, e manipolare testo in maniera efficiente.

## How to: (Come fare:)
```Fish Shell
# Ricerca di parole che iniziano con 'es' e finiscono con 'oni'
echo "espressioni estensioni esami esagoni" | string match -r 'es\w*oni'

# Output:
espressioni
estensioni
```

```Fish Shell
# Sostituzione di 'windows' con 'linux' in una frase
set frase "Preferisco usare windows al posto di linux"
echo $frase | string replace -r 'windows' 'linux'

# Output:
Preferisco usare linux al posto di linux
```

```Fish Shell
# Validazione di indirizzi email formati correttamente
echo "mario.rossi@example.com ciao@example m.rossi@ex@ample.com" | string match -r '([A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6})'

# Output:
mario.rossi@example.com
```

## Deep Dive (Approfondimento)
Le regex esistono da decenni, evolvendo dai semplici wildcard a sistemi complessi gestiti dai moderni motori regex come quelli di Perl e PCRE (Perl Compatible Regular Expressions). Fish Shell non include un motore regex proprio ma utilizza comandi come `string match`, che usa le regex di sistema. Ci sono alternative come grep, sed, e awk, ma `string` in Fish è più coerente con il suo design moderno e user-friendly. Per l'implementazione, Fish usa regex ERE (Espressioni Regolari Estese) che sono potenti e flessibili.

## See Also (Vedi Anche)
- La documentazione ufficiale di Fish sulla manipolazione di stringhe: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Una guida interattiva alle regex: [regexr.com](https://regexr.com/)
- Il manuale di PCRE per approfondire l’uso delle regex: [pcre.org](https://www.pcre.org/)
