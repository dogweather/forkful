---
title:                "Gleam: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest jednym z najpopularniejszych formatów danych używanych w dzisiejszym świecie programowania. Jest to uniwersalny i łatwy do czytania sposób na przechowywanie i przesyłanie informacji. W artykule tym dowiecie się, dlaczego warto poznać podstawy obsługi JSON w języku Gleam.

## Jak to zrobić

```Gleam
let json = Json.Encode.object([
        ( "name", "John" ),
        ( "age", Json.Encode.int(27) )
      ])

let encoded_json = Json.Encode.encode_pretty(2, json)
```

Wykorzystując powyższy przykład, można utworzyć obiekt JSON, zawierający informacje o imieniu i wieku osoby. Następnie za pomocą metody `encode_pretty` można zakodować ten obiekt w sposób czytelny dla człowieka i wyświetlić go na ekranie.

```
{
  "name": "John",
  "age": 27
}
```

Można również odczytać dane z pliku JSON i przetworzyć je na obiekty w języku Gleam, korzystając z metody `Decode`.

```Gleam
  let json =
    """
    {
      "name": "Emma",
      "age": 34
    }
    """

  case Json.Decode.decodeString(json) {
    Ok(person) ->
      println("Name: " ++ person.name)
      println("Age: " ++ person.age |> String.from_int)
    Error(err) ->
      println("Error: " ++ Json.Decode.error_to_string(err))
  }
```
W powyższym przykładzie odczytujemy dane z obiektu JSON i wyświetlamy informacje o imieniu i wieku osoby. Jeśli wystąpił błąd podczas przetwarzania danych, zostanie wypisany odpowiedni komunikat.

## Głębsza analiza

Obsługa JSON w języku Gleam jest bardzo prosta i intuicyjna. Można wykorzystać wiele metod i funkcji, aby manipulować danymi w formacie JSON. Warto jednak pamiętać, że należy uważać na niezgodności w nazwach kluczy i typach danych przy pracy z różnymi źródłami JSON.

## Zobacz również

- Dokumentacja języka Gleam dotycząca obsługi JSON: https://gleam.run/modules/std/json
- Przykładowy projekt wykorzystujący dane w formacie JSON w języku Gleam: https://github.com/mylesj/json-demo-gleam
- Blog o programowaniu w języku Gleam (w języku polskim): https://gleam-lang.org/blog