---
title:                "Praca z formatem yaml"
html_title:           "PHP: Praca z formatem yaml"
simple_title:         "Praca z formatem yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pracować z formatem YAML w swoich projektach PHP? Po pierwsze, YAML jest niezwykle czytelnym i łatwym w użyciu formatem, który ułatwia tworzenie i zarządzanie konfiguracjami i danymi. Po drugie, jest to popularne rozwiązanie w świecie projektów open-source, dzięki czemu można łatwo znaleźć wsparcie i dokumentację w razie potrzeby.

## Jak to zrobić

### Instalacja biblioteki YAML

Aby móc pracować z formatem YAML w PHP, musimy najpierw zainstalować odpowiednią bibliotekę. W tym celu możemy skorzystać z menedżera pakietów Composer, wykonując w terminalu następującą komendę:

```PHP
composer require symfony/yaml
```

### Tworzenie pliku YAML

Format YAML jest oparty na strukturze klucz-wartość, podobnie jak np. JSON. Możemy więc w łatwy sposób utworzyć plik YAML, przypisując odpowiednie wartości do kluczy. Przykładowy plik YAML może wyglądać następująco:

```PHP
# plik konfiguracyjny
imie: Jan
nazwisko: Kowalski
wiek: 35
zainteresowania:
  - programowanie
  - sport
```

### Odczyt i zapis pliku YAML

Biblioteka YAML pozwala nam na wygodne odczytywanie danych z pliku YAML oraz zapisywanie zmian. Przykładowy kod może wyglądać tak:

```PHP
use Symfony\Component\Yaml\Yaml;

// odczyt danych z pliku
$data = Yaml::parseFile('plik.yaml');

// zapis zmian
$data['kolor'] = 'zielony';
Yaml::dump($data, 'plik.yaml');
```

### Wyświetlanie danych

Przejdźmy teraz do wyświetlania danych z pliku YAML. W tym celu możemy skorzystać z pętli foreach i funkcji echo, aby wyświetlić wszystkie klucze i odpowiadające im wartości:

```PHP
foreach ($data as $key => $value) {
  echo $key . ' - ' . $value . '<br>';
}
```

Powyższy kod wyświetli następujący wynik:

```
imie - Jan
nazwisko - Kowalski
wiek - 35
zainteresowania - Array
kolor - zielony
```

## Deep Dive

Format YAML został stworzony z myślą o tym, aby być łatwym w użyciu i możliwym do czytania przez człowieka, co wyróżnia go na tle innych formatów danych. Poza prostą strukturą klucz-wartość, YAML pozwala na tworzenie bardziej złożonych struktur danych, takich jak tablice, obiekty czy nawet mapy.

Jednym z przydatnych narzędzi w pracy z YAML jest także narzędzie "Validator", które pozwala na sprawdzenie poprawności składni pliku YAML oraz wypisanie błędów, jeśli jakieś występują. Możemy skorzystać z niego w następujący sposób:

```PHP
use Symfony\Component\Yaml\Exception\ParseException;
use Symfony\Component\Yaml\Validator\Validator;

// próba przetworzenia pliku YAML
try {
    Yaml::parseFile('plik.yaml');
} catch (ParseException $exception) {
    echo 'Błąd składni: ' . $exception->getMessage();
}

// sprawdzanie poprawności pliku YAML
$validator = new Validator();
if ($validator->validate('plik.yaml')) {
    echo 'Plik YAML jest poprawny!';
} else {
    echo 'Wystąpiły błędy w pliku YAML.';
}
```

## Zobacz także

- [Dokumentacja biblioteki YAML](https://symfony.com/doc/current/components/yaml.html)
- [Przykładowe projekty wykorzystujące format