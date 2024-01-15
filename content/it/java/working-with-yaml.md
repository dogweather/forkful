---
title:                "Lavorare con yaml"
html_title:           "Java: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Java, potresti aver sentito parlare di YAML ma non sai esattamente perché dovresti iniziare ad usarlo. In realtà, lavorare con YAML può semplificare notevolmente la gestione dei dati e delle configurazioni all'interno dei tuoi progetti. Continua a leggere per scoprire di più su come YAML può essere utilizzato nel tuo codice Java.

## Come fare

La prima cosa da fare è aggiungere la dipendenza di SnakeYAML al tuo progetto Maven:

```Java
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.23</version>
</dependency>
```

Una volta fatto, puoi iniziare a utilizzare la libreria YAML nelle tue classi Java:

```Java
// Importa le classi necessarie
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

// Carica il file YAML
Yaml yaml = new Yaml();
InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream("config.yml");
Map<String, Object> data = yaml.load(inputStream);

// Ottieni i valori dal file YAML
String username = (String) data.get("username");
int port = (int) data.get("port");
```

Ora puoi utilizzare i valori estratti dal file YAML all'interno del tuo codice. Inoltre, puoi anche scrivere oggetti Java in un file YAML usando la classe `DumperOptions`:

```Java
// Crea un oggetto da scrivere nel file YAML
Map<String, Object> user = new LinkedHashMap<>();
user.put("name", "John");
user.put("age", 30);

// Scrivi l'oggetto in un file YAML
Yaml yaml = new Yaml(new DumperOptions());
String output = yaml.dump(user);
```

## Approfondimento

Oltre all'utilizzo di base mostrato sopra, ci sono alcune funzionalità avanzate che possono rendere ancora più utile l'utilizzo di YAML nel tuo progetto Java. Ad esempio, puoi definire diverse sezioni di configurazione all'interno di un unico file YAML e utilizzare il tag `!include` per includere altri file YAML all'interno del primo. Inoltre, puoi utilizzare il tag `!secret` per nascondere informazioni sensibili o riservate all'interno del file YAML.

Per saperne di più su queste funzionalità e altre ancora, puoi consultare la documentazione ufficiale di SnakeYAML o cercare altri tutorial e guide online.

## Vedi anche

- [Documentazione di SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [Esempio dell'utilizzo di SnakeYAML in un progetto Java](https://www.baeldung.com/java-snake-yaml)
- [Guida rapida a YAML](https://yaml.org/start.html)