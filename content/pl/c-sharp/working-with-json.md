---
title:                "Praca z formatem json"
html_title:           "C#: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Co to jest JSON i po co to robić?
JSON (JavaScript Object Notation) jest to lekki i czytelny sposób przedstawiania danych w postaci obiektów tekstowych. Programiści korzystają z JSON, aby łatwiej przekazywać i przechowywać dane, szczególnie w przypadku aplikacji internetowych.

## Jak to zrobić:
### Przedstawienie danych w formacie JSON:
```
C# var person = new { name = "John", age = 30, hobbies = ["reading", "gaming"] };
Console.WriteLine(JsonConvert.SerializeObject(person));
```

### Rezultat:
```
{"name":"John","age":30,"hobbies":["reading","gaming"]}
```

### Odczytanie danych z formatu JSON:
```
C# var json = "{\"name\":\"John\",\"age\":30,\"hobbies\":[\"reading\",\"gaming\"]}";
var person = JsonConvert.DeserializeObject<Person>(json);
Console.WriteLine($"Name: {person.Name} \nAge: {person.Age} \nHobbies: {string.Join(", ", person.Hobbies)}");
```

### Rezultat:
```
Name: John
Age: 30
Hobbies: reading, gaming
```

## Głębsze zgłębienie tematu:
### Kontekst historyczny:
JSON został stworzony przez Douglasa Crockforda w 2001 roku i stał się popularnym sposobem zapisywania i wymiany danych.

### Alternatywy:
Jedną z alternatyw dla JSON jest format XML, który również służy do przechowywania i wymiany danych. Jednak JSON jest bardziej czytelny i łatwiejszy w użyciu dla programistów.

### Szczegóły implementacji:
W języku C# istnieje narzędzie o nazwie *Newtonsoft.Json*, które udostępnia metody do serializacji i deserializacji danych w formacie JSON.

## Zobacz także:
- [Dokumentacja Newtonsoft.Json](https://www.newtonsoft.com/json/help/html/introduction.htm)
- [Inne formaty danych: XML, CSV, YAML](https://stackify.com/json-xml-yaml-csv/)