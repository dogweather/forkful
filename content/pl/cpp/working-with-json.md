---
title:                "C++: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z formatem JSON jest niezbędna podczas programowania w języku C++. Jest to popularny sposób przechowywania i przesyłania danych w aplikacjach webowych oraz mobilnych. W tym wpisie dowiesz się, jak w prosty sposób pracować z JSON w C++, co z pewnością ułatwi Ci pracę w programowaniu.

## Jak

Aby rozpocząć pracę z formatem JSON w C++, musisz najpierw dodać bibliotekę JsonCpp do swojego projektu. Możesz to zrobić za pomocą menedżera pakietów lub ręcznie, poprzez pobranie plików źródłowych. Następnie należy dodać nagłówek "json/json.h" do swojego pliku źródłowego.

Teraz przyjrzyjmy się przykładowemu kodowi, który tworzy obiekt JSON i zapisuje go do pliku:

```C++
#include <iostream>
#include "json/json.h"

int main() {
    Json::Value myObj;
    myObj["name"] = "Jan Kowalski";
    myObj["age"] = 30;
    myObj["hobbies"].append("programowanie");
    myObj["hobbies"].append("jazda na rowerze");

    std::ofstream file("myObject.json");
    file << myObj;
    file.close();
}
```

Kod ten najpierw importuje potrzebne nagłówki, a następnie tworzy obiekt JSON o nazwie "myObj". W kolejnych liniach dodaje do niego klucze i wartości za pomocą operatora "[]". W końcu obiekt ten jest zapisywany do pliku "myObject.json" dzięki wykorzystaniu strumienia plikowego.

Po uruchomieniu tego kodu, utworzony zostanie plik JSON o następującej strukturze:

```JSON
{
    "name": "Jan Kowalski",
    "age": 30,
    "hobbies": [
        "programowanie",
        "jazda na rowerze"
    ]
}
```

Możesz także łatwo odczytać dane z pliku JSON i wyświetlić je na ekranie. Oto przykład kodu z tym zadaniem:

```C++
#include <iostream>
#include "json/json.h"

int main() {
    Json::Value myObj;
    std::ifstream file("myObject.json");
    file >> myObj;

    std::cout << "Imię: " << myObj["name"].asString() << std::endl;
    std::cout << "Wiek: " << myObj["age"].asInt() << std::endl;

    std::cout << "Hobby: ";
    for (int i = 0; i < myObj["hobbies"].size(); i++) {
        std::cout << myObj["hobbies"][i].asString();
        if (i != myObj["hobbies"].size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << std::endl;

    file.close();
}
```

W tym przypadku najpierw importujemy plik JSON, a następnie odczytujemy z niego dane i wyświetlamy je na ekranie.

## Deep Dive

Powyższe przykłady pokazują podstawowe operacje związane z pracą z formatem JSON w C++. Jednakże, istnieje wiele innych funkcji i metod, które mogą okazać się przydatne w bardziej złożonych projektach. Na przykład, możesz wykorzystać pętlę "for" do iterowania po elementach obiektu JSON, lub wyświetlić wszystkie klucze i wartości za pomocą metody "getMemberNames()".

Warto także zauważyć, że biblioteka JsonCpp jest bardzo dobrze udokumentowana, więc jeśli masz jakieś pytania lub wątpliwości, możesz łatwo znaleźć odpowiedź w dokumentacji.

## Zobacz też

- Oficjalna dokumentacja biblioteki JsonCpp: https://github.com/open-source-parsers/jsoncpp/wiki
- Przykładowe projekty wykorzystujące