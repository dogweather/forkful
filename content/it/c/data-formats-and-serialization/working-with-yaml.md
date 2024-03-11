---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:22.530392-07:00
description: "YAML, acronimo di \"YAML Ain't Markup Language\", \xE8 uno standard\
  \ di serializzazione dati leggibile dall'uomo che pu\xF2 essere utilizzato per svariate\u2026"
lastmod: '2024-03-11T00:14:17.550011-06:00'
model: gpt-4-0125-preview
summary: "YAML, acronimo di \"YAML Ain't Markup Language\", \xE8 uno standard di serializzazione\
  \ dati leggibile dall'uomo che pu\xF2 essere utilizzato per svariate\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cosa & Perché?

YAML, acronimo di "YAML Ain't Markup Language", è uno standard di serializzazione dati leggibile dall'uomo che può essere utilizzato per svariate applicazioni, dai file di configurazione alla memorizzazione dei dati. I programmatori spesso lavorano con YAML quando hanno bisogno di un formato facile da leggere e da scrivere per file di configurazione o per lo scambio di dati tra lingue e sistemi.

## Come fare:

Lavorare con YAML in C richiede una libreria, poiché la libreria standard di C non fornisce supporto diretto per l'analisi o la serializzazione di YAML. Una delle librerie YAML più popolari per C è `libyaml`, che offre interfacce a basso e alto livello per l'analisi e la generazione di YAML. Di seguito è riportato un esempio di come analizzare un semplice file YAML utilizzando `libyaml`:

**Prima**, è necessario installare la libreria `libyaml`. Se si utilizza un sistema simile a Unix, è possibile solitamente installarla tramite il gestore di pacchetti. Ad esempio, su Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Successivamente**, prendi in considerazione un semplice file YAML chiamato `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Ecco** un esempio basilare di come analizzare questo file YAML in C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Impossibile inizializzare l'analizzatore YAML!\n", stderr);

    if (fh == NULL)
        fputs("Impossibile aprire il file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Valore: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Questo semplice programma apre un file YAML, inizializza l'analizzatore YAML e legge il file, stampando i valori scalari (in questo esempio, i campi del nostro semplice YAML). Da notare che il controllo degli errori è minimo in questo semplice esempio e dovrebbe essere più robusto in codice di produzione.

L'esecuzione del programma con il nostro `config.yaml` produrrà:

```plaintext
Valore: John Doe
Valore: 29
Valore: false
```

## Approfondimento

YAML è stato rilasciato per la prima volta nel 2001 ed è stato progettato per essere più leggibile e facile da usare rispetto ad altri formati di serializzazione dati come XML o JSON, prendendo ispirazione da vari linguaggi come C, Perl e Python per la sua filosofia di design. Nonostante i suoi vantaggi in termini di leggibilità e facilità di modifica umana, YAML può essere complesso da analizzare programmaticamente a causa della sua dipendenza dall'indentazione e dal suo vasto set di funzionalità, incluse referenze e tipi personalizzati.

Sebbene `libyaml` fornisca un accesso robusto e a basso livello all'analisi e alla generazione di YAML in C, può essere ingombrante per compiti semplici a causa della sua API verbosa. Per questi motivi, alcuni programmatori preferiscono utilizzare librerie di livello superiore o addirittura altri formati di serializzazione dati come JSON quando lavorano in C, specialmente quando l'analisi performante con minimo overhead di codice è una priorità. Tuttavia, YAML rimane una scelta popolare per file di configurazione e situazioni in cui la leggibilità umana è fondamentale. Alternative come TinyYAML o l'incorporazione di un interprete di alto livello (ad esempio, l'incorporamento di Python o Lua) potrebbero fornire maggiore comodità per applicazioni specifiche, bilanciando tra facilità d'uso e necessità di prestazioni.
