---
title:                "Praca z yaml"
html_title:           "TypeScript: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, który korzysta z JavaScript lub TypeScript, na pewno słyszałeś o formacie YAML. YAML to język oparty na tekście, który jest coraz częściej wykorzystywany do przechowywania i przesyłania danych w aplikacjach webowych. W tym artykule dowiesz się, dlaczego warto nauczyć się pracować z YAML.

## Jak to zrobić

Na początek musimy zainstalować pakiet @types/yaml, który umożliwia używanie TypeScript z YAML. W terminalu wpisz następującą komendę:

```TypeScript
npm install @types/yaml
```
Następnie importujemy pakiet w naszym kodzie TypeScript:

```TypeScript
import * as YAML from 'yaml'
```
Mamy teraz dostęp do funkcji YAML, które pozwalają nam na łatwe parsowanie i serializację danych.

Oto przykładowe użycie YAML do przetworzenia danych w formacie JSON:

```TypeScript
const jsonExample = {
  name: 'Jan Kowalski',
  age: 25,
  job: 'programista'
}

const yamlStr = YAML.stringify(jsonExample)

console.log(yamlStr)

/*
Output:
name: Jan Kowalski
age: 25
job: programista
*/
```

Możemy również używać YAML do przechowywania konfiguracji w naszych aplikacjach. Jest to bardzo wygodny sposób na przechowywanie danych w postaci strukturalnej i późniejsze łatwe dostęp do nich. Przykładowo, możemy stworzyć plik "config.yaml" z następującymi danymi:

```YAML
database:
  host: localhost
  port: 3306
  user: admin
  password: 123456
```

A następnie w naszym kodzie użyć YAML do odczytania tych danych:

```TypeScript
const config = YAML.parse(fs.readFileSync('./config.yaml', 'utf-8'))

console.log(config.database.host) // Output: localhost
```

## Głębszy wgląd

Należy zauważyć, że YAML jest bardzo elastycznym formatem i możemy używać go do przechowywania różnych typów danych, takich jak tablice, obiekty itp. Możemy również zmieniać format danych w dowolnym momencie, ponieważ YAML jest formatem tekstu.

Warto również zwrócić uwagę na to, że wiele narzędzi programistycznych obsługuje YAML, co może być bardzo przydatne przy pracy nad projektami zespołowymi.

## Zobacz również

- Oficjalna dokumentacja biblioteki YAML dla TypeScript: https://www.npmjs.com/package/@types/yaml
- Przewodnik po składni YAML: https://yaml.org/spec/1.2/spec.html
- Przykładowe projekty wykorzystujące format YAML: https://github.com/search?q=yaml&type=Repositories