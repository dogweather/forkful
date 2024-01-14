---
title:                "Java: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un'applicazione Java, è probabile che ti troverai a lavorare con dati in formato JSON. Questo formato è diventato sempre più popolare per la sua capacità di rappresentare dati in modo leggibile e compatto. 

## Come Fare

Ci sono diverse librerie Java disponibili per lavorare con JSON, ma in questo articolo useremo la libreria Gson di Google per mostrare come analizzare e creare dati in formato JSON.

Per prima cosa, dobbiamo importare la libreria Gson nel nostro progetto. Possiamo farlo aggiungendo la seguente dipendenza al nostro file pom.xml:

```
<dependency>
    <groupId>com.google.code.gson</groupId>
    <artifactId>gson</artifactId>
    <version>2.8.6</version>
</dependency>
```

Una volta importata la libreria, possiamo iniziare a lavorare con JSON. Di seguito è riportato un esempio di codice che mostra come analizzare una stringa JSON e accedere ai suoi valori:

```java
// importiamo le classi necessarie
import com.google.gson.Gson;
import com.google.gson.JsonObject;

// definiamo una stringa JSON di esempio
String jsonString = "{\"nome\": \"Marco\", \"cognome\": \"Rossi\", \"eta\": 30}";

// creiamo un oggetto Gson
Gson gson = new Gson();

// analizziamo la stringa JSON in un oggetto JsonObject
JsonObject jsonObject = gson.fromJson(jsonString, JsonObject.class);

// accediamo ai valori all'interno dell'oggetto
String nome = jsonObject.get("nome").getAsString();
String cognome = jsonObject.get("cognome").getAsString();
int eta = jsonObject.get("eta").getAsInt();

// stampiamo i valori
System.out.println("Nome: " + nome);
System.out.println("Cognome: " + cognome);
System.out.println("Età: " + eta);

// output:
// Nome: Marco
// Cognome: Rossi
// Età: 30
```

Possiamo anche creare un oggetto JSON a partire da una classe Java, come mostrato nell'esempio seguente:

```java
// definiamo una classe per rappresentare una persona
public class Persona {
    private String nome;
    private String cognome;

    public Persona(String nome, String cognome) {
        this.nome = nome;
        this.cognome = cognome;
    }

    // getter e setter
}

// creiamo un oggetto Persona
Persona persona = new Persona("Gianna", "Verdi");

// creiamo un oggetto Gson
Gson gson = new Gson();

// convertiamo l'oggetto in una stringa JSON
String jsonString = gson.toJson(persona);

// stampiamo la stringa JSON
System.out.println(jsonString);

// output: {"nome": "Gianna", "cognome": "Verdi"}
```

## Approfondimento

Oltre alle operazioni di base di analisi e creazione di JSON mostrate sopra, ci sono molte altre cose interessanti che puoi fare con la libreria Gson. Ad esempio, puoi gestire tipi di dati complessi come array e oggetti nidificati utilizzando le classi `JsonArray` e `JsonObject`. Oppure puoi definire le tue proprie funzioni di deserializzazione e serializzazione per personalizzare il modo in cui Gson gestisce i dati. Consulta la documentazione ufficiale di Gson per saperne di più.

## Vedi Anche

- [Documentazione di Gson](https://github.com/google/gson/blob/master/UserGuide.md)
- [Tutorial su JSON in Java](https://www.baeldung.com/java-json)
- [Java JSON Processing API](https://www.oracle.com/technical-resources/articles/java/json.html)