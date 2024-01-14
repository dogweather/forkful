---
title:                "Java: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest jednym z najpopularniejszych formatów służących do przechowywania i wymiany danych w aplikacjach. Jest to lekki, czytelny dla człowieka i łatwy do parsowania format, dlatego jest często używany w programowaniu. Jeśli jesteś programistą Java i chcesz nauczyć się pracować z JSON, ten artykuł jest dla Ciebie.

## Jak to zrobić

Parsowanie i generowanie JSON w języku Java jest bardzo proste dzięki wbudowanej klasie ```JSONObject```. Aby rozpocząć pracę z JSON, musimy najpierw pobrać bibliotekę JSON-Java ze strony http://json.org/java/. Następnie należy dodać pobraną bibliotekę do projektu.

Aby parsować istniejący JSON, należy utworzyć nowy obiekt klasy ```JSONObject``` i przekazać mu tekst zawierający dane w formacie JSON. Następnie możemy korzystać z różnych metod, takich jak ```get()``` lub ```getString()```, aby pobrać potrzebne nam wartości z obiektu. Przykładowy kod wyglądałby tak:

```Java
String jsonStr = "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":28}";
JSONObject jsonObject = new JSONObject(jsonStr);

String firstName = jsonObject.getString("firstName");
String lastName = jsonObject.getString("lastName");
int age = jsonObject.getInt("age");

System.out.println("First Name: " + firstName);
System.out.println("Last Name: " + lastName);
System.out.println("Age: " + age);
```

Wyjście tego kodu byłoby następujące:

```
First Name: John
Last Name: Doe
Age: 28
```

Podobnie, aby wygenerować nowy obiekt w formacie JSON, możemy utworzyć nowy obiekt ```JSONObject``` i powiązać z nim odpowiednie klucze i wartości. Następnie możemy użyć metody ```toString()``` aby zamienić obiekt na tekst w formacie JSON. Przykładowy kod wyglądałby tak:

```Java
JSONObject jsonObject = new JSONObject();
jsonObject.put("firstName", "Jane");
jsonObject.put("lastName", "Smith");
jsonObject.put("age", 32);

String jsonStr = jsonObject.toString();
System.out.println(jsonStr);
```

Wyjście tego kodu byłoby następujące:

```
{"firstName":"Jane","lastName":"Smith","age":32}
```

## Pogłębiona analiza

Klasa ```JSONObject``` zawiera również wiele innych przydatnych metod, które pozwalają na bardziej zaawansowane operacje na danych w formacie JSON. Na przykład, możemy użyć metody ```put()``` aby dodać nowy klucz i wartość do istniejącego obiektu, lub użyć metody ```remove()``` aby usunąć dany klucz i wartość. Klasa ta również umożliwia obsługę tablic w formacie JSON poprzez użycie obiektów klasy ```JSONArray```.

Korzystając z tych metod oraz wyrażeń warunkowych i pętli, można zaimplementować zaawansowane operacje na danych w formacie JSON, co czyni język Java idealnym narzędziem do pracy z tym formatem.

## Zobacz również

Ta lista linków może Ci pomóc w dalszej nauce:

- Oficjalna strona formatu JSON: http://json.org/
- Oficjalna dokumentacja klasy JSONObject: https://static.javadoc.io/org.json/json/20180130/org/json/JSONObject.html
- Wideo-tutorial o pracy z JSON w języku Java: https://www.youtube.com/watch?v=N6Z6SueRYcA