---
title:                "Swift: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z formatem YAML jest nieodłączną częścią programowania w języku Swift, ponieważ jest to popularny sposób zapisywania i przetwarzania danych. Dzięki temu narzędziu możemy w łatwy sposób przechowywać i udostępniać informacje w naszych programach.

## Jak to zrobić

Aby rozpocząć pracę z YAML w Swift, musimy najpierw zaimportować odpowiednią bibliotekę. Możemy to zrobić przy użyciu komendy `import YAML` w naszym kodzie. Następnie, możemy utworzyć nowy obiekt typu `YAML` i przypisać do niego nasze dane w postaci tekstu lub tablicy.

```Swift
import YAML

let data = "imie: Jan\nwiek: 30\nzawod: programista"
let yaml = YAML(data)
```

Teraz, gdy mamy nasze dane zapisane w formacie YAML, możemy je przetworzyć i wyświetlić przy użyciu metod dostępnych w bibliotece. Na przykład, jeśli chcemy wyświetlić imię osoby z naszych danych, możemy wywołać metodę `string(for:)` i przekazać jako argument odpowiedni klucz.

```Swift
let name = yaml.string(for: "imie") // "Jan"
print(name)
```

## Głębsze zagłębienie

Język YAML jest bardzo elastyczny i obsługuje różne typy danych, takie jak tekst, liczby, tablice czy nawet obiekty. W naszym przykładzie, zapisaliśmy dane w postaci tekstowej, ale gdybyśmy chcieli użyć tablicy, moglibyśmy to zrobić w następujący sposób:

```Swift
let array = ["jabłko", "banan", "truskawki"]
let yaml = YAML(array)
let fruits = yaml.array // ["jabłko", "banan", "truskawki"]
```

Dodatkowo, biblioteka YAML oferuje wiele przydatnych metod do przetwarzania danych w tym formacie. Możemy na przykład sprawdzić, czy dany klucz istnieje w naszych danych przy użyciu metody `contains(key:)` lub wyświetlić wszystkie klucze zawarte w obiekcie za pomocą metody `allKeys()`.

## Zobacz także

- [Dokumentacja biblioteki YAML](https://github.com/behrang/YamlSwift)
- [Oficjalna strona formatu YAML](https://yaml.org/)