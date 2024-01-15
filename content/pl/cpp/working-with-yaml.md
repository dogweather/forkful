---
title:                "Praca z yaml"
html_title:           "C++: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Dlaczego

Jeśli jesteś programistą pracującym z językiem C++, na pewno spotkałeś się z formatem plików YAML. Jest to bardzo popularny format wykorzystywany do przechowywania danych w sposób czytelny dla człowieka i łatwy do przetwarzania przez komputery. W tym artykule dowiesz się, dlaczego warto pracować z YAML i jak zacząć.

## Jak zacząć

Aby rozpocząć pracę z YAML, musisz najpierw pobrać bibliotekę YAML-cpp. Możesz to zrobić korzystając z menedżera pakietów lub bezpośrednio ze strony projektu na GitHub. Następnie musisz dodać ją do swojego projektu poprzez włączenie odpowiednich plików nagłówkowych i linkowanie z biblioteką. Teraz jesteś gotowy, aby zacząć korzystać z YAML.

Przykładowy kod, który wczytuje plik YAML i wyświetla jego zawartość:

```C++
#include <iostream>
#include "yaml-cpp/yaml.h"

using namespace std;

int main()
{
    // wczytanie pliku
    YAML::Node config = YAML::LoadFile("config.yml");

    // wyświetlenie zawartości
    cout << "Witaj " << config["imie"].as<string>() << "!" << endl;
    cout << "Twój rekord w grze wynosi: " << config["wynik"].as<int>() << endl;

    return 0;
}
```

## Głębsze zanurzenie

Podczas pracy z YAML warto zwrócić uwagę na kilka ważnych rzeczy:

- Struktura pliku: plik YAML składa się z kluczy i wartości ułożonych w formie drzewa. Wartości są przydzielane do kluczy za pomocą dwukropka i spacji. Pamiętaj o zachowaniu odpowiedniej struktury pliku.
- Typy danych: YAML obsługuje wiele typów danych, takich jak ciągi znaków, liczby i tablice. Upewnij się, że przypisujesz odpowiednie typy danych do swoich kluczy.
- Ogólna czytelność: YAML jest stworzony z myślą o czytelności dla człowieka. Dlatego pamiętaj o odpowiednim formatowaniu pliku, aby był on łatwy w odczycie i zrozumieniu.

# Zobacz również

- [Dokumentacja YAML-cpp](https://github.com/jbeder/yaml-cpp/wiki)
- [Oficjalna strona formatu YAML](https://yaml.org)
- [Przykładowe projekty wykorzystujące YAML](https://github.com/topics/yaml)