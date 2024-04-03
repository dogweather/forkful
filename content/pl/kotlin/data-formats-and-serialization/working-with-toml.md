---
date: 2024-01-26 04:23:52.289727-07:00
description: "Jak to zrobi\u0107: Aby obs\u0142ugiwa\u0107 TOML w Kotlinie, mo\u017C\
  na u\u017Cy\u0107 biblioteki takiej jak `ktoml`. Na pocz\u0105tek, dodajmy zale\u017C\
  no\u015B\u0107 w pliku `build.gradle.kts`."
lastmod: '2024-03-13T22:44:35.388583-06:00'
model: gpt-4-0125-preview
summary: "Aby obs\u0142ugiwa\u0107 TOML w Kotlinie, mo\u017Cna u\u017Cy\u0107 biblioteki\
  \ takiej jak `ktoml`."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Aby obsługiwać TOML w Kotlinie, można użyć biblioteki takiej jak `ktoml`. Na początek, dodajmy zależność w pliku `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Teraz, zobaczmy jak przetworzyć TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("Host Bazy Danych: $host")
    println("Port Bazy Danych: $port")
}
```

Zakładając, że `config.toml` wygląda tak:

```toml
[database]
host = "localhost"
port = 5432
```

Przykładowy wynik:

```
Host Bazy Danych: localhost
Port Bazy Danych: 5432
```

## Dogłębna analiza
TOML, stworzony przez współzałożyciela GitHub, Toma Preston-Wernera w 2013 roku, miał być bardziej przejrzysty niż YAML i bezpieczniejszy typowo niż JSON. Zyskał popularność, szczególnie z systemem pakietów Rust'a `Cargo` oraz systemem modułów Go. Alternatywy? YAML posiada więcej funkcji, JSON bezpośrednio przekłada się na obiekty w wielu językach programowania, a zawsze jest jeszcze stary dobry XML. Jeśli chodzi o implementację, ktoml, na licencji Apache 2.0, to czysta biblioteka Kotlinowa i nie przyciąga za sobą bibliotek Javy, oferując również DSL-e do pisania w TOML, nie tylko do czytania.

## Zobacz również
- GitHub TOML: https://github.com/toml-lang/toml
- GitHub ktoml: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
