---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Che cos'è e perché?

Inviare una richiesta HTTP è un modo con cui il tuo programma può comunicare con i server web e accedere alle risorse online. I programmatori lo fanno per recuperare dati, inviare dati, testare API e molto altro ancora.

# Come si fa:

Ecco come inviare una richiesta HTTP GET con `curl` in Fish Shell. Prima, digitare `curl` seguito dall'URL richiesto.

```fish
curl https://api.github.com/users/octocat
```

Ecco l'output tipico:

```json
{
  "login": "octocat",
  "id": 1,
  "node_id": "MDQ6VXNlcjE=",
  ...
}
```

# Approfondimento

Le richieste HTTP sono un principio fondamentale del web moderno. Il formato della richiesta HTTP è stato definito negli anni '80 e il protocollo continua ad evolvere, un esempio recente è l'HTTP/2.

Anche se `curl` è uno strumento comune per inviare richieste HTTP, ci sono molte altre alternative come `wget` o `httpie`. Inoltre, la maggior parte dei linguaggi di programmazione ha librerie per inviare richieste HTTP, come `requests` in Python.

Nell'usare la Fish Shell per inviare richiesta HTTP, `curl` esegue la richiesta e stampa il risultato nel terminale. Puoi anche fare di più come salvare l'output in un file o gestire i header HTTP.

# Vedi Anche

- Documentazione HTTP da Mozilla: https://developer.mozilla.org/it/docs/Web/HTTP
- Manuale di curl: https://curl.haxx.se/docs/manpage.html
- Documentazione Fish Shell: https://fishshell.com/docs/current/index.html
- httpie, un'alternativa user-friendly a curl: https://httpie.io/docs
- Documentazione delle librerie requests di Python: https://docs.python-requests.org/en/latest/