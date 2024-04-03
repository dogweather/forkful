---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:13.715898-07:00
description: "Come fare: Rimbocchiamoci le maniche e iniziamo a programmare con JSON\
  \ in Java. Prima cosa, avrai bisogno di una libreria di elaborazione JSON come\u2026"
lastmod: '2024-03-13T22:44:43.330307-06:00'
model: gpt-4-0125-preview
summary: Rimbocchiamoci le maniche e iniziamo a programmare con JSON in Java.
title: Lavorare con JSON
weight: 38
---

## Come fare:
Rimbocchiamoci le maniche e iniziamo a programmare con JSON in Java.

Prima cosa, avrai bisogno di una libreria di elaborazione JSON come `Jackson` o `Google Gson`. Qui useremo `Jackson`, quindi aggiungi questa dipendenza al tuo `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Ora, serializziamo (scriviamo) un semplice oggetto Java in JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person persona = new Person("Alex", 30);
            String json = mapper.writeValueAsString(persona);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String nome;
    public int eta;

    public Person(String nome, int eta) {
        this.nome = nome;
        this.eta = eta;
    }
}
```

L'output dovrebbe essere:

```json
{"nome":"Alex","eta":30}
```

Ora, per deserializzare (leggere) il JSON di nuovo in un oggetto Java:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"nome\":\"Alex\",\"eta\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person persona = mapper.readValue(json, Person.class);
            System.out.println(persona.nome + " ha " + persona.eta + " anni.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

L'output sarà:

```
Alex ha 30 anni.
```

## Approfondimento
La semplicità e l'efficacia di JSON lo hanno reso lo standard de facto per lo scambio di dati sulla rete, superando XML. Introdotta nei primi anni 2000, JSON deriva da JavaScript ma ora è supportato dalla maggior parte dei linguaggi.

Alternative a JSON includono XML, che è più verboso, e formati binari come Protocol Buffers o MessagePack, che sono meno leggibili dall'uomo ma più efficienti in termini di dimensione e velocità. Ognuno ha i suoi casi d'uso; la scelta dipende dalle specifiche esigenze dati e dal contesto.

In Java, oltre a `Jackson` e `Gson`, abbiamo `JsonB` e `org.json` come altre librerie per gestire JSON. Jackson offre un'elaborazione basata su stream ed è noto per la velocità, mentre Gson è celebrato per la sua facilità di uso. JsonB è parte di Jakarta EE, offrendo un approccio più standardizzato.

Quando si implementa JSON, ricordatevi di gestire correttamente le vostre eccezioni - il vostro codice dovrebbe essere robusto contro input errati. Inoltre, considerate le implicazioni di sicurezza del binding automatico dei dati – validate sempre i vostri input!

## Vedi Anche
- [Progetto Jackson](https://github.com/FasterXML/jackson)
- [Progetto Gson](https://github.com/google/gson)
- [Specifiche JSON](https://www.json.org/json-en.html)
- [Specifiche JsonB](https://jakarta.ee/specifications/jsonb/)
