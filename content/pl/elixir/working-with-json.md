---
title:                "Praca z json"
html_title:           "Elixir: Praca z json"
simple_title:         "Praca z json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Praca z JSON jest niezbędnym elementem dla współczesnych programistów. JSON (JavaScript Object Notation) to popularny format danych, który jest wykorzystywany w wielu aplikacjach internetowych i mobilnych. Jest bardzo prosty w użyciu i ma wydajną strukturę, dlatego jest często wybierany jako typ danych do przechowywania i przekazywania informacji.

## Jak to zrobić:

Aby pracować z JSON w Elixir, wystarczy użyć wbudowanej biblioteki :jason. Przykładowo, jeśli chcemy przetworzyć JSON w pliku, możemy użyć funkcji File.read i Jason.decode:

```Elixir
json = File.read("dane.json")
{:ok, parsed_json} = Jason.decode(json)
```

Aby przetworzyć JSON z zewnętrznego źródła, na przykład za pomocą API, możemy użyć modułu Httpoison, który umożliwia przekazanie odpowiedzi jako JSON do funkcji Jason.decode.

```Elixir
response = Httpoison.get("https://api.example.com")
{:ok, parsed_json} = Jason.decode(response.body)
```

Aby utworzyć własny JSON, można użyć funkcji Jason.encode:

```Elixir
my_data = %{"name" => "John", "age" => 25, "city" => "New York"}
encoded_json = Jason.encode(my_data)
```

## Głębsze zanurzenie:

Format JSON został stworzony w 2001 roku przez Douglasa Crockforda i jest bardzo popularny wśród programistów JavaScript. W związku z tym, że jest platformowo- i języko-niezależny, jest szeroko stosowany w różnych projektach. Alternatywnie, można użyć innych formatów, takich jak XML lub CSV, jednak JSON oferuje prostszą i bardziej czytelną strukturę.

Biblioteka :jason opiera się na wydajnej bibliotece C, więc jest szybka i niezawodna w przetwarzaniu dużej ilości danych. 

## Zobacz także:

- [Instrukcja obsługi JSON w Elixir](https://elixir-lang.org/getting-started/json.html)
- [Oficjalna dokumentacja biblioteki :jason](https://hexdocs.pm/jason/Jason.html)
- [Popularne formaty danych używane w programowaniu](https://www.programiz.com/article/json-xml-csv-data-formats)