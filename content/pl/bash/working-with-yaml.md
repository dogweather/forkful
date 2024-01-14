---
title:                "Bash: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą pracującym w języku Bash, prawdopodobnie słyszałeś o plikach YAML. YAML, czyli "Yet Another Markup Language", jest językiem formatowania danych, który jest wykorzystywany do przechowywania i przesyłania danych w czytelnej dla człowieka formie. W tym artykule, dowiesz się dlaczego warto poznać ten język i jak zacząć pracować z plikami YAML.

## Jak To Zrobić

Aby zacząć pracę z YAML w języku Bash, musisz najpierw zainstalować odpowiednią bibliotekę o nazwie 'yq'. Możesz to zrobić za pomocą menedżera pakietów swojego systemu operacyjnego lub ręcznie pobrać i skompilować bibliotekę. Po zainstalowaniu, możesz zacząć korzystać z poleceń i funkcji zawartych w bibliotece 'yq'.

Poniżej przedstawiamy przykłady kodu Bash wykorzystującego bibliotekę yq do pracy z plikiem YAML.

Pobranie wartości ze struktury YAML:
```Bash
$ yq e '.info.version' config.yaml
1.0.0
```

Dodanie lub zmiana wartości w pliku YAML:
```Bash
$ yq e '.database.username = "john"' config.yaml
```

Odczytanie całej zawartości pliku YAML:
```Bash
$ yq e '.' config.yaml
```

Dzięki bibliotece yq możesz również zmieniać i filtrować dane w pliku YAML w bardziej zaawansowany sposób. Dokumentacja i przykłady znajdują się na oficjalnej stronie projektu.

## Wnikliwe Zagłębienie

Jedną z najważniejszych cech YAML jest jego czytelność dla człowieka. Struktura danych przypomina tę zapisywaną w języku JSON, jednak YAML jest znacznie bardziej czytelny i przejrzysty. Ponadto, dzięki funkcjom biblioteki yq, możesz w prosty sposób manipulować danymi w plikach YAML, co może być bardzo pomocne w automatyzacji procesów i zarządzaniu konfiguracją.

Ponieważ YAML jest językiem formatowania danych, jest wykorzystywany w wielu różnych dziedzinach, takich jak tworzenie konfiguracji serwerów, przetwarzanie informacji w chmurze, czy tworzenie skryptów testowych. Dlatego warto poznać ten język i umiejętność pracy z nim może być przydatna w przyszłych projektach.

## Zobacz Również
- Dokumentacja biblioteki yq: https://kislyuk.github.io/yq/
- Oficjalna strona języka YAML: https://yaml.org/
- Przewodnik po języku YAML: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/

Dziękujemy za przeczytanie tego artykułu i życzymy sukcesów w pracy z YAML w języku Bash!