---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con autenticazione di base in Fish Shell

## Cos'è e perché?

L'invio di una richiesta HTTP con autenticazione di base è un mezzo per accedere a risorse Web protette da una semplice forma di identificazione. I programmatori lo fanno per interactare con API che richiedono un livello base di sicurezza.

## Come fare

Inviamo una richiesta GET ad esempio, con `curl` e basic auth: 

```Fish Shell 
set username 'tuo_nome_utente'
set password 'tua_password'
curl -u $username:$password https://esempio.com
```

Se l'invio è andato a buon fine, la risposta HTTP comprenderà i dati richiesti.

## Approfondimento 

Questa forma di autenticazione è stato introdotta dagli standard HTTP nel 1996. Da allora, è rimasta un elemento fondamentale delle operazioni HTTP, anche se esistono metodi alternativi più sicuri, come l'autenticazione digest e l'autenticazione basata su token.

L'uso di `curl` in Fish Shell per l'invio delle richieste HTTP è una soluzione pratica. Il comando `-u` di `curl` si occupa di applicare l'header di autenticazione corretto (Authorization) alla richiesta HTTP. 

Tenga presente, però, che le vostre credenziali sono inviate in chiaro con la Basic auth, quindi è importante assicurarsi che la connessione sia protetta con SSL/TLS.

## Vedi Anche

Per ulteriori informazioni sull’autenticazione di base, si veda la specifica della RFC 7617 a questo [link](https://tools.ietf.org/html/rfc7617). 

Altro sul comando `curl` e le sue variabili opzioni si può trovare a questo [link](https://curl.haxx.se/).

Per esplorare metodi alternativi di autenticazione HTTP, consultate questa [guida](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication).