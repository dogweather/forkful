---
title:                "Praca z YAML"
html_title:           "Javascript: Praca z YAML"
simple_title:         "Praca z YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pracować z YAML? Otóż jest to prosty i elastyczny format dla przesyłania danych, który jest bardzo popularny w świecie programowania. Pozwala na łatwe zapisywanie struktury danych w czytelnej dla człowieka formie.

## Jak to zrobić

Kodowanie w YAML jest bardzo proste i intuicyjne, a dzięki temu może być używane przez programistów o różnym poziomie zaawansowania. Poniżej znajdziesz kilka przykładów kodu i wyników, aby zapoznać się z podstawami:

### Tworzenie obiektu YAML
```Javascript
const person = {
  name: "Kasia",
  age: 25,
  city: "Warszawa"
};

const yamlString = YAML.stringify(person);
console.log(yamlString);
```
### Wynik
```
name: Kasia
age: 25
city: Warszawa
```

### Tworzenie listy YAML
```Javascript
const fruits = ["Jabłko", "Banan", "Truskawka", "Pomarańcza"];
const yamlList = YAML.stringify(fruits);
console.log(yamlList);
```
### Wynik
```
- Jabłko
- Banan
- Truskawka
- Pomarańcza
```

### Wczytywanie danych z pliku YAML
```Javascript
const fs = require('fs');
const yamlData = fs.readFileSync('data.yml', 'utf8');
const data = YAML.parse(yamlData);
console.log(data);
```
### Wynik
```
{
  name: "Adam",
  age: 30,
  city: "Kraków"
}
```

## Głębsza analiza

Tworzenie obiektów i list w YAML jest bardzo intuicyjne, jednak warto pamiętać o kilku ważnych szczegółach:

- Obiekty w YAML są tworzone przy użyciu par klucz-wartość, gdzie dwukropek oddziela klucz od wartości.
- Listy w YAML są tworzone przy użyciu myślników, a każdy element jest zapisywany w nowej linii.
- Plik YAML powinien być zapisany w formacie UTF-8, aby uniknąć problemów z kodowaniem.

Możesz też wykorzystać zalety YAML w swoim projekcie, np. jako format konfiguracyjny dla swojej aplikacji.

## Zobacz też

- Dokumentacja YAML: https://yaml.org/
- Biblioteka YAML dla Javascript: https://eemeli.org/yaml/#home
- Artykuł "10 powodów dlaczego warto używać YAML": https://codeenigma.com.au/blog/10-reasons-why-yaml-rules