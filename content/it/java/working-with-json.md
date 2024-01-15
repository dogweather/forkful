---
title:                "Lavorare con json"
html_title:           "Java: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un'applicazione in Java che richiede la comunicazione con API esterne o la memorizzazione di dati nel cloud, è probabile che dovrai gestire i dati in formato JSON. Questo linguaggio di markup leggero è diventato lo standard de facto per lo scambio di dati via HTTP ed è ampiamente supportato dalle librerie di Java.

## Come fare

Per iniziare a lavorare con JSON in Java, è necessario importare la libreria Gson (Google's JSON library) nel proprio progetto. Una volta importata, è possibile utilizzare la classe Gson per convertire oggetti Java in formato JSON e viceversa.

**Codice di esempio:**

```Java
import com.google.gson.Gson;

public class Main {
    public static void main(String[] args) {
        // Creazione di un oggetto Java
        Person person = new Person("Sarah", "Smith", 25);

        // Conversione in JSON
        Gson gson = new Gson();
        String json = gson.toJson(person);

        // Stampa del JSON
        System.out.println(json);

        // Conversione da JSON a oggetto Java
        Person newPerson = gson.fromJson(json, Person.class);

        // Stampa dei dati dell'oggetto Java
        System.out.println(newPerson.getFirstName());
        System.out.println(newPerson.getLastName());
        System.out.println(newPerson.getAge());
    }
}

// Definizione della classe Person
class Person {
    private String firstName;
    private String lastName;
    private int age;

    // Costruttore
    public Person(String firstName, String lastName, int age) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.age = age;
    }

    // Metodi Getter e Setter
    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }
}
```

**Output:**

{"firstName":"Sarah", "lastName":"Smith", "age":25}

Sarah
Smith
25

## Approfondimento

Oltre alla classe Gson, esistono altre librerie di terze parti disponibili per lavorare con JSON in Java, come ad esempio Jackson e JSON-java. Inoltre, è possibile anche utilizzare le annotazioni di Gson per personalizzare la conversione tra oggetti Java e JSON, evitando così la scrittura manuale del codice.

Inoltre, è importante tenere conto della struttura del JSON con cui si sta lavorando e delle corrispondenti classi Java che si dovranno creare. Ad esempio, se il JSON contiene una lista di oggetti, sarà necessario creare una classe che rappresenta ciascun oggetto e una classe che rappresenta la lista.

## Vedi anche

- [Documentazione ufficiale di Gson](https://github.com/google/gson/blob/master/UserGuide.md)
- [Esempi di utilizzo di Gson](https://mkyong.com/java/how-do-convert-java-object-to-from-json-format-gson-api/)
- [Introduzione al parsing JSON in Java](https://stackify.com/parse-json-java/)