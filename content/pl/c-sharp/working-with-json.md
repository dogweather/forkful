---
title:                "Praca z json"
html_title:           "C#: Praca z json"
simple_title:         "Praca z json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Możliwość manipulacji danymi w formacie JSON jest niezbędna dla wielu projektów programistycznych. JSON jest powszechnie stosowany w aplikacjach internetowych, aplikacjach mobilnych, bazach danych i wiele więcej.

## Jak To Zrobić

### Tworzenie JSON

Tworzenie obiektów JSON w C# jest proste i intuicyjne. Wystarczy użyć klasy `JObject` z przestrzeni nazw`Newtonsoft.Json` oraz metody `Add` do dodawania właściwości i ich wartości.

```C#
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

JObject jsonObject = new JObject();
jsonObject.Add("name", "John");
jsonObject.Add("age", 30);
jsonObject.Add("hobbies", new JArray("gaming", "reading", "traveling"));
```

### Serializacja do formatu JSON

Aby przekonwertować dane z C# na format JSON, można wykorzystać metodę `JsonConvert.SerializeObject` z przestrzeni nazw `Newtonsoft.Json`.

```C#
string json = JsonConvert.SerializeObject(jsonObject);
Console.WriteLine(json);
// Output: {"name":"John","age":30,"hobbies":["gaming","reading","traveling"]}
```

### Deserializacja z formatu JSON

Aby przekonwertować dane z formatu JSON na obiekty C#, można wykorzystać metodę `JsonConvert.DeserializeObject` z przestrzeni nazw `Newtonsoft.Json`.

```C#
string json = @"{
    'name': 'John',
    'age': 30,
    'hobbies': ['gaming', 'reading', 'traveling']
}";

JObject jsonObject = JsonConvert.DeserializeObject<JObject>(json);
string name = jsonObject["name"].ToObject<string>();
int age = jsonObject["age"].ToObject<int>();
JArray hobbies = jsonObject["hobbies"].ToObject<JArray>();
```

### Przeszukiwanie i modyfikowanie danych JSON

Dzięki wykorzystaniu metody `JsonConvert.DeserializeObject`, możemy przekonwertować dane w formacie JSON na obiekty, które mogą być łatwo przeszukiwane i modyfikowane.

```C#
string json = @"{
    'name': 'John',
    'age': 30,
    'hobbies': ['gaming', 'reading', 'traveling']
}";

Person person = JsonConvert.DeserializeObject<Person>(json);
Console.WriteLine("Name: " + person.Name); // Output: Name: John
Console.WriteLine("Age: " + person.Age); // Output: Age: 30
Console.WriteLine("Hobbies: " + string.Join(", ", person.Hobbies)); // Output: Hobbies: gaming, reading, traveling

// Modyfikacja danych
person.Name = "Bill";
person.Hobbies.Add("coding");
Console.WriteLine(person.Name); // Output: Bill
Console.WriteLine(string.Join(", ", person.Hobbies)); // Output: gaming, reading, traveling, coding
```

## Głębsze Zagadnienia

- W jaki sposób radzić sobie z błędami podczas przetwarzania danych JSON?
- Jak wykorzystać atrybuty do serializacji i deserializacji obiektów do formatu JSON?
- Jak przekonwertować dane JSON na typy niestandardowe (np. daty)?
- W jaki sposób uprościć kod do pracy z danymi JSON przy pomocy bibliotek takich jak `Newtonsoft.Json.Linq`?

## Zobacz także

- Dokumentacja oficjalna JSON na stronie https://www.json.org
- Przewodnik po tworzeniu i obsłudze danych JSON w C# https://docs.microsoft.com/pl-pl/dotnet/standard/serialization/system-text-json-how-to
- Przykłady użycia biblioteki `Newtonsoft.Json` https://www.newtonsoft.com/json/help/html/Introduction.htm