---
title:                "Praca z yaml"
html_title:           "Kotlin: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Praca z YAML to sposób definiowania danych w formacie tekstowym, który jest czytelny dla człowieka. Programiści wykorzystują go do przechowywania i przesyłania danych w swoich aplikacjach, ponieważ jest łatwy do zrozumienia i współpracuje z wieloma językami programowania.

## Jak to zrobić:
Kotlin jest językiem programowania wspierającym YAML dzięki bibliotece SnakeYAML. Aby użyć jej w swoim projekcie, należy dodać odpowiednią zależność do pliku build.gradle:
```
dependencies {
    implementation("org.yaml:snakeyaml:1.27")
}
```
Następnie można użyć klasy Yaml do parsowania lub tworzenia danych YAML, np.:
```
val data = """
        name: John Smith
        age: 30
        occupation: Programmer
    """.trimIndent()

    val yaml = Yaml()
    val obj = yaml.load(data)

    println(obj["name"]) // John Smith
```

## Głębsza wiedza:
YAML (Yet Another Markup Language) został stworzony w 2001 roku i jest inspirowany formatami takimi jak XML czy JSON. Oprócz biblioteki SnakeYAML, istnieją też inne narzędzia do pracy z YAML w Kotlinie, np. kotlinx.serialization.

## Zobacz też:
- [Dokumentacja biblioteki SnakeYAML dla Kotlina](https://bitbucket.org/asomov/snakeyaml/wiki/Home)
- [Oficjalna strona YAML](https://yaml.org/)
- [Kotlinx.serialization - narzędzie do serializowania danych w YAML](https://github.com/Kotlin/kotlinx.serialization)