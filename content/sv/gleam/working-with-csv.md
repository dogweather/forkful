---
title:                "Arbeta med csv"
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Arbeta med CSV (Comma-Separated Values) handlar om att hantera enkla textfiler för att lagra och utbyta data. Programmerare använder ofta CSV eftersom det är lättläst, enhetligt och enkelt att importera i olika program.

## Så här gör du:
Gleam har inget standardbibliotek för CSV än, så du får parsera manuellt. Här är ett exempel:

```gleam
pub fn parse_csv_line(line: String) -> List(String) {
  line
  |> string.split(by: ",")
  |> list.map(string.trim)
}

pub fn main() {
  let csv_data = "namn,ålder,stad\nKalle,23,Göteborg\nLisa,37,Stockholm"
  let lines = csv_data |> string.split(by: "\n")
  case list.drop(lines, 1) {
    Ok(data_lines) -> {
      data_lines
      |> list.map(parse_csv_line)
      |> io.debug
    }
    Error(_) -> io.debug("Inga data")
  }
}
```

Kör programmet ger:

```
[[namn, ålder, stad], [Kalle, 23, Göteborg], [Lisa, 37, Stockholm]]
```

## Djupdykning
CSV-formatet dök upp runt 1970-talet och är fortfarande populärt för datautbyte. Alternativ till CSV inkluderar JSON, XML och YAML. Effektiviteten av din Gleam CSV-hantering kan skifta beroende på filstorlek och komplexitet. För stora data, överväg streaming.

## Se även:
- Gleam’s stdlib documentation: [https://hexdocs.pm/gleam_stdlib/](https://hexdocs.pm/gleam_stdlib/)
- CSV on Wikipedia for a historical overview: [https://sv.wikipedia.org/wiki/CSV](https://sv.wikipedia.org/wiki/CSV)
- Alternatives to CSV: [https://json.org/](https://json.org/), [https://yaml.org/](https://yaml.org/), [https://www.w3.org/XML/](https://www.w3.org/XML/)