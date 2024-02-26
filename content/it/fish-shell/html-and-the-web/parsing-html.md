---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:05.874360-07:00
description: "Il parsing di HTML si riferisce all'estrazione di dati o informazioni\
  \ da contenuti HTML, un compito comune quando si gestiscono dati web. I programmatori\u2026"
lastmod: '2024-02-25T18:49:41.700020-07:00'
model: gpt-4-0125-preview
summary: "Il parsing di HTML si riferisce all'estrazione di dati o informazioni da\
  \ contenuti HTML, un compito comune quando si gestiscono dati web. I programmatori\u2026"
title: Analisi del HTML
---

{{< edit_this_page >}}

## Che cosa & Perché?

Il parsing di HTML si riferisce all'estrazione di dati o informazioni da contenuti HTML, un compito comune quando si gestiscono dati web. I programmatori fanno ciò per automatizzare l'estrazione di informazioni dai siti web, per compiti come il web scraping, il data mining o il testing automatizzato.

## Come fare:

La shell Fish, prevalentemente, non è progettata per il parsing HTML direttamente. Tuttavia, eccelle nel collegare insieme strumenti Unix come `curl`, `grep`, `sed`, `awk`, o nell'utilizzo di strumenti specializzati come `pup` o `beautifulsoup` in uno script Python. Di seguito sono riportati esempi che illustrano come sfruttare questi strumenti all'interno della shell Fish per fare il parsing di HTML.

### Utilizzando `curl` e `grep`:
Recuperare contenuti HTML ed estrarre le righe che contengono link:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Output:
```
/page1.html
/page2.html
...
```

### Utilizzando `pup` (uno strumento da riga di comando per il parsing di HTML):

Prima, assicurati che `pup` sia installato. Poi puoi usarlo per estrarre elementi tramite i loro tag, id, classi, ecc.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Output, simile all'esempio `grep`, elencherà gli attributi href dei tag `<a>`.

### Con uno script Python e `beautifulsoup`:

Anche se Fish di per sé non può fare il parsing di HTML nativamente, si integra perfettamente con gli script Python. Di seguito è riportato un esempio conciso che utilizza Python con `BeautifulSoup` per fare il parsing ed estrarre titoli da HTML. Assicurati di avere `beautifulsoup4` e `requests` installati nel tuo ambiente Python.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Utilizzo:

```fish
parse_html 'https://example.com'
```

Output:
```
Esempio di dominio
```

Ciascuno di questi metodi serve casi d'uso e scale di complessità diverse, dalla semplice manipolazione di testo da riga di comando al pieno potere di parsing di `beautifulsoup` negli script Python. A seconda delle tue esigenze e della complessità della struttura HTML, potresti scegliere un semplice pipeline Unix o un approccio di scripting più potente.
