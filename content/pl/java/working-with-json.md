---
title:                "Praca z formatem json"
html_title:           "Java: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego 

JSON (JavaScript Object Notation) jest powszechnie wykorzystywanym formatem wymiany danych w programowaniu. Jest on lekki, czytelny dla człowieka i łatwy w użyciu. W tym artykule dowiesz się, dlaczego warto znać i umieć pracować z JSON w języku Java.

## Jak 

Aby pracować z JSON w języku Java, będziemy potrzebować dwóch bibliotek: `json-simple` i `Gson`. Pierwsza z nich jest lekka i łatwa w użyciu, natomiast druga obsługuje bardziej zaawansowane opcje. Poniżej przedstawione są przykłady kodów oraz wyjścia dla obu bibliotek.

### Użycie `json-simple`:
```Java
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

JSONObject obj = new JSONObject();
obj.put("imie", "Ania");
obj.put("wiek", 25);
obj.put("hobby", "programowanie");

System.out.println(obj);
```

Wyjście:
```Java
{"imie":"Ania","wiek":25,"hobby":"programowanie"}
```
### Użycie `Gson`:
```Java
import com.google.gson.Gson;

class Osoba {
    String imie;
    int wiek;
    String hobby;
}

Osoba osoba = new Osoba();
osoba.imie = "Ania";
osoba.wiek = 25;
osoba.hobby = "programowanie";

Gson gson = new Gson();
String json = gson.toJson(osoba);

System.out.println(json);
```

Wyjście:
```Java
{"imie":"Ania","wiek":25,"hobby":"programowanie"}
```

## Deep Dive

W przypadku bardziej zaawansowanych operacji na JSON, warto skorzystać z biblioteki `Gson`. Pozwala ona na łatwe mapowanie obiektów Java na JSON i odwrotnie, a także obsługę bardziej złożonych struktur danych. Ponadto, można również skorzystać z klas `JsonReader` i `JsonWriter`, aby czytać i pisać dane JSON bezpośrednio z pliku.

Warto również zapoznać się z różnymi formatami danych, które są obsługiwane przez JSON, np. tekst, liczby, tablice czy obiekty. W przypadku pracy z dużymi i złożonymi strukturami danych, warto również zwrócić uwagę na optymalizację i wydajność operacji na JSON.

## Zobacz także

- Dokumentacja biblioteki `json-simple`: https://code.google.com/archive/p/json-simple/
- Dokumentacja biblioteki `Gson`: https://github.com/google/gson
- Tutorial na temat pracy z JSON w języku Java: https://www.mkyong.com/java/how-do-convert-java-object-to-from-json-format-gson-api/