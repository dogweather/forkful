---
date: 2024-01-26 04:22:56.627481-07:00
description: "TOML sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio\
  \ di Tom). \xC8 un formato di serializzazione dei dati utilizzato per i file di\u2026"
lastmod: '2024-03-13T22:44:43.332408-06:00'
model: gpt-4-0125-preview
summary: TOML sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio di
  Tom).
title: Lavorare con TOML
weight: 39
---

## Come fare:
Avrai bisogno di una libreria di parsing TOML. Io consiglio `toml4j`. Aggiungilo al tuo progetto così:

```java
// Aggiungi questo al tuo build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Ecco come si effettua il parsing di un file TOML:

```java
import com.moandjiezana.toml.Toml;

public class EsempioToml {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("IP del Server: " + ip);
        System.out.println("Porta del Server: " + port);
    }
}
```

Esempio di output:

```
IP del Server: 192.168.1.1
Porta del Server: 80
```

## Approfondimento
Sviluppato dal cofondatore di GitHub, Tom Preston-Werner, TOML mirava ad essere più semplice dell'XML e più specificato dello YAML. La sua ultima versione 1.0.0, rilasciata nel 2021, offre un insieme stabile di funzionalità.

Alternative come JSON o YAML sono anche popolari. JSON è ottimo per lo scambio di dati. YAML è più leggibile dall'uomo per configurazioni complesse. La forza di TOML sta nella sua semplicità e nel suo utilizzo nella comunità Rust.

Per quanto riguarda l'implementazione, quando si utilizza TOML con Java, è importante tenere a mente che il parser scelto ha la sua importanza. Oltre a `toml4j`, alcuni optano per `jackson-dataformat-toml`. Ognuno avrà le sue sfumature, come la gestione degli errori o la performance di parsing, quindi scegli in base alle esigenze del tuo progetto.

## Vedi Anche
- Specifica TOML: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
