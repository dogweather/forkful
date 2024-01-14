---
title:                "Rust: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt hantera YAML-filer i dina Rust-program? Kanske för att läsa in eller spara konfigurationsdata? Eller kanske för att läsa in data från någon annan YAML-baserad tjänst? Oavsett vilket, så kommer kunskap om hur man arbetar med YAML definitivt att vara till nytta för dig som Rust-utvecklare. Låt oss ta en titt på hur man gör det!

## Så här gör du

För att kunna hantera YAML-filer i ditt Rust-program behöver du ett externt bibliotek som kan tolka YAML för dig. Det finns flera olika bibliotek som du kan välja mellan, men för denna guide kommer vi att använda "serde_yaml". 

Först måste du inkludera biblioteket i ditt projekt genom att lägga till följande rad i ditt "Cargo.toml"-fil:

```
[dependencies]
serde_yaml = "0.8.11"
```

När det är gjort kan du börja använda biblioteket i ditt program. Vi börjar med att importera bibliotekets funktioner genom att lägga till följande rad i början av vår "main.rs"-fil:

```
use serde_yaml;
```

För att läsa in en YAML-fil kan vi använda funktionen "from_reader" som tar en "BufReader" som argument och returnerar en "Result"-typ som antingen är ett "Yaml"-objekt eller ett felmeddelande. Låt oss titta på ett exempel:

```Rust
let file = std::fs::File::open("config.yml")?;
let reader = std::io::BufReader::new(file);

let yaml_result = serde_yaml::from_reader(reader)?;

// Gör något med yaml_resultatet här ...
```

Som du kan se använder vi standardbiblioteket för att öppna och läsa in filen "config.yml". Sedan använder vi "from_reader"-funktionen från "serde_yaml"-biblioteket för att tolka YAML-innehållet och returnera ett "Yaml"-objekt. Om allt går som det ska, kan vi sedan göra vad vi vill med "yaml_result"-objektet.

För att spara en YAML-fil kan vi använda funktionen "to_writer" som tar en "BufWriter" och ett "Yaml"-objekt som argument. Låt oss se ett exempel på detta också:

```Rust
let config = MyConfig { /* ... */ };
let file = std::fs::File::create("config.yml")?;
let writer = std::io::BufWriter::new(file);

serde_yaml::to_writer(writer, &config)?;
```

Här skapar vi först en instans av vår "MyConfig"-struktur och sedan öppnar och skapar filen "config.yml" för skrivning. Sedan använder vi "to_writer"-funktionen från "serde_yaml"-biblioteket för att skriva vår "MyConfig"-struktur som YAML till filen.

## Djupdykning

Nu när du har lärt dig grunderna för hur man hanterar YAML-filer i Rust, låt oss titta på några andra användbara funktioner som finns tillgängliga i "serde_yaml"-biblioteket. Du kan till exempel använda funktionen "from_slice" för att läsa in YAML-data från en "u8"-buffert eller funktionen "to_vec" för att konvertera ett "Yaml"-objekt till en "u8"-buffert. Du kan även använda "to_string" och "from_str" för strängrepresentationer av YAML-data.

Ett annat användbart verktyg är "Value"-strukturen som låter dig arbeta med YAML som ett träd av värden, liknande hur JSON hanteras i Rust. Du kan också använda egenskaper som "into_vec" och "as_mapping" för att få mer flexibilitet i hur du använder "Value"-strukturen.

Det finns också ett stort antal attribut som du kan använda för att anpassa hur "serde_yaml" tolkar och skriver ut ditt YAML-innehåll. För mer information om detta, kolla in dokumentationen för "serde_yaml".

## Se även

Förutom "serde_yaml" finns det flera andra bibli