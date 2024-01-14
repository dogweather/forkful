---
title:                "Java: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-json.md"
---

{{< edit_this_page >}}

# Warum

JSON ist eine sehr nützliche Form von Daten, die in vielen Bereichen der Softwareentwicklung verwendet wird. Es ist ein einfaches und leicht lesbares Format, das für den Austausch von Daten zwischen Anwendungen verwendet werden kann. Egal, ob Sie eine Webanwendung entwickeln, Daten analysieren oder mit API-Anfragen arbeiten, die Wahrscheinlichkeit ist hoch, dass Sie irgendwann mit JSON konfrontiert werden. Daher ist es wichtig, eine grundlegende Kenntnis davon zu haben, wie man mit JSON in Java arbeitet.

# Wie man mit JSON in Java arbeitet

Die Java-Plattform bietet verschiedene Bibliotheken und APIs, mit denen Sie problemlos mit JSON arbeiten können. Im Folgenden werden wir uns auf die Verwendung der Standardbibliothek von Java konzentrieren.

Zunächst müssen Sie ein JSON-Objekt erstellen. Dazu können Sie die Klasse JsonObject aus der Java.util-Bibliothek verwenden. Das folgende Beispiel zeigt, wie Sie ein JSON-Objekt erstellen und einige Werte hinzufügen können:

```Java
import java.util.*;

public class JSONDemo {

     public static void main(String[] args) {
         
         // Erstelle ein neues JSON-Objekt
         JsonObject jsonObject = new JsonObject();
         
         // Füge einige Werte hinzu
         jsonObject.addProperty("Name", "Max Mustermann");
         jsonObject.addProperty("Alter", 25);
         jsonObject.addProperty("Beruf", "Softwareentwickler");
         
         // Gib das JSON-Objekt aus
         System.out.println(jsonObject);
         
     }

}
```

Die Ausgabe dieses Codes wäre:

```JSON
{
  "Name": "Max Mustermann",
  "Alter": 25,
  "Beruf": "Softwareentwickler"
}
```

Um auf die Werte eines JSON-Objekts zuzugreifen, können Sie die Methode get(String key) verwenden, wobei der Schlüssel des Werts als Argument übergeben wird. Im folgenden Beispiel wird auf den Namen aus dem oben erstellten JSON-Objekt zugegriffen:

```Java
// Gib den Namen aus dem JSON-Objekt aus
System.out.println(jsonObject.get("Name"));
```

Die Ausgabe wäre:

```JSON
Max Mustermann
```

# Tiefer Einblick in die Arbeit mit JSON

Neben der Erstellung von JSON-Objekten können Sie auch JSON-Arrays erstellen, die eine Liste von JSON-Objekten darstellen. Dazu können Sie die Klasse JsonArray aus der Java.util-Bibliothek verwenden. Im folgenden Beispiel wird ein JSON-Array erstellt und mit zwei JSON-Objekten befüllt:

```Java
import java.util.*;

public class JSONDemo {

     public static void main(String[] args) {
         
         // Erstelle ein neues JSON-Array
         JsonArray jsonArray = new JsonArray();
         
         // Füge JSON-Objekte hinzu
         jsonArray.add(new JsonObject().addProperty("Name", "Max Mustermann").addProperty("Beruf", "Softwareentwickler"));
         jsonArray.add(new JsonObject().addProperty("Name", "Lisa Müller").addProperty("Beruf", "Datenanalystin"));
         
         // Gib das JSON-Array aus
         System.out.println(jsonArray);
     }

}
```

Die Ausgabe dieses Codes wäre:

```JSON
[
  {
    "Name": "Max Mustermann",
    "Beruf": "Softwareentwickler"
  },
  {
    "Name": "Lisa Müller",
    "Beruf": "Datenanalystin"
  }
]
```

Es ist auch möglich, ein JSON-Objekt in eine Java-Map zu konvertieren, um einfacher auf die Werte zuzugreifen. Dazu können Sie die Methode getAsMap() verwenden. Das folgende Beispiel zeigt, wie man das oben erstellte JSON-Objekt in eine Map konvertieren kann:

```Java
// Konvertiere das JSON-Objekt in eine Java-Map
Map<String, Object> map = jsonObject.getAsMap();

// Gib den Namen aus der Map aus
System.out.println(map.get("Name"));
```

Dies würde die Ausgabe "Max Mustermann" erzeugen.

# Siehe auch

- [JSON in Java verwenden](https://www.baeldung.com/java-json)
- [Java.util-Bibliothek](https://docs.oracle.com/javase/8/docs/api/java/util/package-summary.html)
- [JSON-Referenz](https://www.json.org/json-de.html)