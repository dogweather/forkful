---
title:                "Utilizzo di un debugger"
date:                  2024-01-26T03:49:14.640282-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Usare un debugger è fondamentalmente come fare il detective nel proprio codice, cercando di scoprire i bug e capire perché le cose non funzionano bene. I programmatori lo fanno perché, ammettiamolo, i bug sono inevitabili e eliminarli efficacemente significa far funzionare il proprio codice più velocemente e in modo più affidabile.

## Come fare:
Attualmente, Gleam si appoggia all'ecosistema Erlang per gli strumenti, quindi di solito si fa debug con strumenti come `rebar3`, `observer` e `debugger`. Ecco come sporcarti le mani con il debug:

```gleam
// Nel tuo file di configurazione rebar, assicurati di avere queste righe per includere le informazioni di debug:
{erl_opts, [debug_info]}.

// Esegui una shell Erlang con la tua app caricata
rebar3 shell

// All'interno della shell, puoi avviare il debugger
1> debugger:start().
```

Semplice, vero? Si apre la GUI del `debugger` e puoi impostare breakpoint, eseguire passo dopo passo il codice e osservare le variabili quanto vuoi. Non vedrai direttamente il codice Gleam, ma il codice Erlang in cui è compilato, il che è comunque piuttosto utile.

## Approfondimento
Gleam è un linguaggio giovane, quindi, sebbene si appoggi sull'ecosistema Erlang, gli strumenti di debug nativi Gleam non sono ancora sotto i riflettori. Ciò significa che stiamo usando gli strumenti collaudati di Erlang, e questo non è un male. Il debugger di Erlang esiste dagli anni '90, perfezionato negli anni eliminando fastidiosi bug in sistemi dove l'affidabilità è fondamentale.

Per quanto riguarda le alternative, il tracing è un metodo potente nel mondo BEAM (cioè la macchina virtuale che esegue codice Erlang e Elixir). Utilizzando `rebar3` puoi accedere a strumenti come `recon` per tracciare le chiamate di funzione e approfondire i problemi di performance.

Il passaggio dalla scrittura in Gleam al debug in Erlang potrebbe sembrare come se stessi traducendo i tuoi pensieri al volo. Ma il vantaggio è che ottieni un'occhiata al mondo di Erlang, capendo i mattoni della tua app nella sua forma di runtime.

## Vedi Anche
Per espandere il tuo kit di strumenti per il debug, controlla:

- La documentazione del debugger di Erlang: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- La libreria `recon` per Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- Sul tracing nel BEAM: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)