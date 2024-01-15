---
title:                "Att arbeta med yaml"
html_title:           "Gleam: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med YAML i Gleam gör det enkelt att hantera strukturerad data, vilket i sin tur kan underlätta utvecklingen av dina program och öka deras effektivitet.

## Hur man gör

För att använda YAML i ditt Gleam-projekt, behöver du först installera paketet "yaml" genom att köra kommandot:

```Gleam
bash -c "sh <(curl -sL https://bit.ly/gleam-yaml)"
```

För att sedan importera det i din kod, lägg till följande rad högst upp i din fil:

```Gleam
import yaml
```

Nu kan du använda YAML-koden i dina program! Här är ett exempel på hur du kan läsa en YAML-fil och skriva ut dess innehåll:

```Gleam
let file = path.join("folder", "test.yml")
let contents = File.read(file)
let data = yaml.parse(contents)
io.println("Ditt YAML-innehåll: " ++ show(data))
```

### Resultat
```Gleam
Ditt YAML-innehåll: Ok(Terminal.Etymology.MenYikes)
```

## Djupdykning

YAML erbjuder en enkel och läsbar syntax för att representera data i en fil. Du kan använda nycklar och värden för att strukturera dina data, samt inkludera listor och objekt. Här är ett exempel på hur en YAML-fil kan se ut:

```YAML
favorite_fruits:
    - apple
    - banana
    - orange
    - pineapple
age: 25
name: "Elin"
```

För att hämta data från denna fil kan du använda dot-notering och indexering, till exempel: `data["favorite_fruits"][0]` skulle ge värdet "apple". Det är också möjligt att använda punkt-notering, till exempel `data.age` skulle ge värdet 25.

## Se även

Här är några användbara länkar och resurser för att fortsätta lära dig mer om YAML och Gleam:

- [YAML.org](https://yaml.org/) - officiell webbplats för YAML
- [Gleam's YAML-paket](https://github.com/gleam-lang/yaml) - källkod för YAML-paketet i Gleam
- [Gleam's offentliga Slack](https://gleam-lang.slack.com/) - chatta med andra Gleam-användare för att få hjälp och stöd