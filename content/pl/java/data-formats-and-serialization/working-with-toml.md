---
title:                "Praca z TOML"
aliases:
- /pl/java/working-with-toml/
date:                  2024-01-26T04:23:16.652484-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
TOML oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalny Język Toma). Jest to format serializacji danych używany w plikach konfiguracyjnych. Programiści używają go, ponieważ jest łatwy do odczytu, zapisu i dobrze odwzorowuje się na tablicę mieszającą.

## Jak to zrobić:
Będziesz potrzebować biblioteki do parsowania TOML. Polecam `toml4j`. Dodaj ją do swojego projektu w taki sposób:

```java
// Dodaj to do swojego build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Oto jak przetworzyć plik TOML:

```java
import com.moandjiezana.toml.Toml;

public class PrzykladToml {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [serwer]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("serwer.ip");
        Integer port = toml.getLong("serwer.port").intValue();
        
        System.out.println("IP serwera: " + ip);
        System.out.println("Port serwera: " + port);
    }
}
```

Przykładowy wynik:

```
IP serwera: 192.168.1.1
Port serwera: 80
```

## Dogłębna analiza
Stworzony przez współzałożyciela GitHub, Toma Preston-Wernera, TOML miał na celu być prostszy niż XML i bardziej sprecyzowany niż YAML. Jego najnowsza wersja 1.0.0, wydana w 2021 roku, oferuje stabilny zestaw funkcji.

Alternatywy takie jak JSON czy YAML są również popularne. JSON jest świetny do wymiany danych. YAML jest bardziej czytelny dla człowieka przy skomplikowanych konfiguracjach. Siłą TOML jest jego prostota i użycie w społeczności Rust.

Jeśli chodzi o implementację, używając TOML w Javie, należy pamiętać, że wybór parsera ma znaczenie. Poza `toml4j`, niektórzy wybierają `jackson-dataformat-toml`. Każdy z nich będzie miał swoje niuanse, takie jak obsługa błędów czy wydajność parsowania, więc wybierz zgodnie z potrzebami twojego projektu.

## Zobacz również
- Specyfikacja TOML: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
