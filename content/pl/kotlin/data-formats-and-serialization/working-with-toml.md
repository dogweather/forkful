---
date: 2024-01-26 04:23:52.289727-07:00
description: "TOML oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalistyczny\
  \ J\u0119zyk Toma). Jest u\u017Cywany dla plik\xF3w konfiguracyjnych, poniewa\u017C\
  \ jest \u0142atwy do\u2026"
lastmod: '2024-02-25T18:49:33.752864-07:00'
model: gpt-4-0125-preview
summary: "TOML oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalistyczny\
  \ J\u0119zyk Toma). Jest u\u017Cywany dla plik\xF3w konfiguracyjnych, poniewa\u017C\
  \ jest \u0142atwy do\u2026"
title: Praca z TOML
---

{{< edit_this_page >}}

## Co i dlaczego?
TOML oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalistyczny Język Toma). Jest używany dla plików konfiguracyjnych, ponieważ jest łatwy do odczytu i zapisu dla ludzi, jednocześnie pozostając łatwym do analizy dla maszyn. Programiści sięgają po TOML, aby uniknąć bałaganu XML i zawiłości JSON, kiedy mają do czynienia z konfiguracjami.

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
