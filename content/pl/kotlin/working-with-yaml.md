---
title:                "Kotlin: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego warto pracować z YAML?

Już nie musisz być programistą, aby usłyszeć o YAML! Jest to język znaczników, który jest coraz popularniejszy w świecie IT. Pozwala on na przechowywanie i przesyłanie danych w sposób uporządkowany i czytelny dla ludzi. Jest szczególnie przydatny w przypadku konfiguracji aplikacji lub serwisów sieciowych. Dzięki temu wpisowi dowiesz się, dlaczego warto poznać YAML i jak zacząć z nim pracować.

## Jak zacząć pracę z YAML w języku Kotlin?

Pierwszym krokiem do pracy z YAML w języku Kotlin jest dodanie odpowiedniej biblioteki do swojego projektu. Możesz zrobić to za pomocą menedżera pakietów w swoim środowisku programistycznym lub ręcznie dodać ją do swojego pliku `build.gradle`.

Następnie musisz utworzyć obiekt `Yaml` z tej biblioteki, aby móc wczytywać i zapisywać dane w formacie YAML. Oto przykładowy kod:

```Kotlin
// Tworzymy obiekt Yaml
val yaml = Yaml()
// Definiujemy dane do zapisania w formacie YAML
val data = mapOf(
    "imie" to "Kamil",
    "wiek" to 30,
    "hobby" to listOf("programowanie", "sport")
)
// Zapisujemy dane do pliku `dane.yaml`
File("dane.yaml").writeText(yaml.dump(data))
```

Teraz możemy odczytać dane z pliku `dane.yaml` za pomocą poniższego kodu:

```Kotlin
// Wczytujemy dane z pliku `dane.yaml`
val dane: Map<*, *> = yaml.load(File("dane.yaml").inputStream()) as Map<*, *>
// Wyświetlamy dane na konsoli
println(dane)
```

**Output:**
```
{imie=Kamil, wiek=30, hobby=[programowanie, sport]}
```

## Zagłębiamy się w świat YAML

Teraz, gdy już wiesz, jak zacząć pracę z YAML w języku Kotlin, czas poznać nieco więcej na jego temat. YAML jest językiem znaczników, więc możesz tworzyć struktury danych w sposób dużo bardziej czytelny dla ludzi niż w przypadku formatów takich jak XML czy JSON. Możesz również wykorzystywać różne typy danych, w tym częściowe i zagnieżdżone mapy oraz listy.

Pamiętaj, że YAML jest językiem wrażliwym na wcięcia, więc dobrze jest trzymać się jednego standardu w swoim kodzie.

Teraz już wiesz, dlaczego warto poznać YAML i jak zacząć z nim pracować w języku Kotlin. Powodzenia w dalszej nauce!

## Zobacz także

- [Dokumentacja biblioteki SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Home)
- [Przykłady wykorzystania YAML w języku Kotlin](https://github.com/ntfalls/kotlin-yaml-example)
- [Tutorial o YAML i jego składni](https://www.tutorialspoint.com/yaml/index.htm)