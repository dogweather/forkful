---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, JavaScript Object Notation, er et dataformat for strukturert info. Programmerere bruker JSON for å lagre og utveksle data enkelt mellom servere og apps.

## How to:
For å jobbe med JSON i C++, trenger du et bibliotek som `nlohmann/json`. Her er en enkel kode:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Parse JSON string
    std::string jsonString = R"({"name":"Ola","age":30,"city":"Oslo"})";
    nlohmann::json jsonObj = nlohmann::json::parse(jsonString);

    // Endre verdier
    jsonObj["city"] = "Bergen";

    // Skriv ut endret JSON
    std::cout << jsonObj.dump(2) << std::endl;
    return 0;
}
```

Sample output:
```
{
  "age": 30,
  "city": "Bergen",
  "name": "Ola"
}
```

## Deep Dive:
JSON dukket opp tidlig på 2000-tallet, raskt adoptert som et enklere alternativ til XML. Andre alternativer inkluderer YAML og TOML. I C++, bruker mange `nlohmann/json` fordi det er enkelt og effektivt, men det er også andre biblioteker som `jsoncpp` og `rapidjson`. Biblioteker innfører overhead, men de gjør JSON-håndtering enkel og feilsikker.

## See Also:
- Offisiell `nlohmann/json` GitHub-side: https://github.com/nlohmann/json
- JSON standard spesifikasjon: https://www.json.org/json-en.html
- `jsoncpp` GitHub-side: https://github.com/open-source-parsers/jsoncpp
- `rapidjson` GitHub-side: https://github.com/Tencent/rapidjson
- TutorialsPoint JSON tutorial: https://www.tutorialspoint.com/json/index.htm
