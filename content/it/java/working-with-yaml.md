---
title:                "Java: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

YAML è un formato di dati leggibile dall'uomo e facilmente parsabile da una macchina. Consente agli sviluppatori di organizzare facilmente le configurazioni e i dati strutturati in un formato semplice e intuitivo.

## Come fare

Per utilizzare YAML in Java, è necessario prima importare la libreria SnakeYAML. Ecco un esempio di codice che illustra come scrivere un file YAML e leggerne il contenuto:

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.io.FileInputStream;
import java.util.Map;

public class Main{
   public static void main(String[] args) throws Exception{
       //Creazione del file YAML
       Yaml yaml = new Yaml();
       String file = "config.yaml";
       Map<String, Object> data = yaml.load(new FileInputStream(file));
       
       //Lettura del file YAML
       String server = (String) data.get("server");
       int port = (int) data.get("port");
       System.out.println("Server: " + server);
       System.out.println("Porta: " + port);
   }
}
```

## Approfondimenti

Esistono diverse librerie Java per lavorare con YAML, come ad esempio SnakeYAML, YamlBeans e JavaProperties. Ognuna di queste offre funzionalità uniche per facilitare la gestione dei dati YAML.

Inoltre, è possibile utilizzare l'annotazione @YAMLProperty per mappare automaticamente i dati YAML su un oggetto Java. Questo rende il processo di lettura e scrittura dei file YAML ancora più semplice ed efficiente.

## Vedi anche

- [Documentazione SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [Esempi di utilizzo di Java con YAML](https://www.baeldung.com/java-snake-yaml)