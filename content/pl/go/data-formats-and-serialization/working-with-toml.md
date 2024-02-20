---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:57.652611-07:00
description: "TOML (Tom's Obvious, Minimal Language) to format pliku konfiguracyjnego,\
  \ kt\xF3ry jest \u0142atwy do odczytania dzi\u0119ki swojej prostej sk\u0142adni.\
  \ Programi\u015Bci u\u017Cywaj\u0105\u2026"
lastmod: 2024-02-19 22:04:54.063746
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) to format pliku konfiguracyjnego,\
  \ kt\xF3ry jest \u0142atwy do odczytania dzi\u0119ki swojej prostej sk\u0142adni.\
  \ Programi\u015Bci u\u017Cywaj\u0105\u2026"
title: Praca z TOML
---

{{< edit_this_page >}}

## Co i dlaczego?

TOML (Tom's Obvious, Minimal Language) to format pliku konfiguracyjnego, który jest łatwy do odczytania dzięki swojej prostej składni. Programiści używają TOML do konfigurowania ustawień aplikacji i zależności ze względu na jego przejrzystość i bezpośrednie mapowanie na struktury danych, co czyni go popularnym wyborem w wielu projektach Go do ustawiania i zarządzania konfiguracjami.

## Jak to zrobić:

Aby zacząć pracować z TOML w Go, najpierw musisz dołączyć bibliotekę, która może analizować pliki TOML, ponieważ standardowa biblioteka Go nie obsługuje TOML natywnie. Pakiet `BurntSushi/toml` jest popularnym wyborem w tym przypadku. Najpierw upewnij się, że go zainstalowałeś:

```bash
go get github.com/BurntSushi/toml
```

Oto prosty przykład, jak go używać. Załóżmy, że masz plik konfiguracyjny o nazwie `config.toml` z następującą zawartością:

```toml
title = "Przykład TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Teraz musisz stworzyć strukturę Go, która odzwierciedla strukturę TOML:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Tytuł: %s\n", config.Title)
    fmt.Printf("Serwer bazy danych: %s\n", config.Database.Server)
}
```

Przykładowe wyjście:

```
Tytuł: Przykład TOML
Serwer bazy danych: 192.168.1.1
```

## Dogłębna analiza

TOML został stworzony przez Toma Preston-Wernera, jednego ze współzałożycieli GitHub, aby zaoferować prosty format pliku konfiguracyjnego, który można łatwo zamapować na tabelę hash i zrozumieć od razu, bez wcześniejszej wiedzy na temat formatu. Kontrastuje to z JSON lub YAML, które, chociaż również szeroko używane, mogą być mniej przyjazne dla ludzi w przypadku plików konfiguracyjnych ze względu na nawiasy, cudzysłowy i problemy z wcięciami.

Pakiet `BurntSushi/toml` w Go to solidna biblioteka, która umożliwia nie tylko dekodowanie, ale także kodowanie plików TOML, czyniąc go wszechstronnym wyborem dla aplikacji, które muszą czytać i pisać pliki konfiguracyjne w tym formacie. Jednak należy zauważyć, że z rozwojem technologii i wprowadzeniem nowych wersji Go pojawiły się alternatywy, takie jak `pelletier/go-toml`, oferujące lepszą wydajność i dodatkowe funkcje, takie jak manipulacja drzewem i wsparcie zapytań.

Chociaż TOML jest świetnym wyborem dla wielu aplikacji, w zależności od złożoności konfiguracji aplikacji i osobistych lub zespołowych preferencji, inne formaty, takie jak YAML lub JSON, mogą być bardziej odpowiednie, zwłaszcza jeśli konfiguracja wymaga bardziej złożonych struktur danych, które werbalna natura TOML może nie uchwycić w elegancki sposób. Niemniej jednak, dla prostych, czytelnych i łatwo edytowalnych konfiguracji, TOML w połączeniu z silnym systemem typów Go i wspomnianymi bibliotekami, jest doskonałym wyborem.
