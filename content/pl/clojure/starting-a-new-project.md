---
title:    "Clojure: Rozpoczynanie nowego projektu"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

##Dlaczego

Tworzenie nowego projektu w Clojure może być świetną opcją dla programistów, którzy chcą pracować w funkcyjnym języku programowania o bogatych możliwościach. Clojure jest prosty w nauce i pozwala na szybkie tworzenie skalowalnych aplikacji. 

##Jak zacząć

Aby rozpocząć nowy projekt w Clojure, najpierw musisz pobrać i zainstalować Clojure na swoim komputerze. Następnie możesz użyć narzędzia Leiningen, aby utworzyć nowy projekt z domyślną strukturą.

```Clojure
lein new nazwa-projektu
```

Wygeneruje to katalog projektu z plikami źródłowymi, również w Clojure, już gotowymi do edycji. Następnie możesz uruchomić projekt, przechodząc do katalogu projektu i wywołując polecenie `lein run`. 

```Clojure
cd nazwa-projektu
lein run
```

Po uruchomieniu projektu, powinieneś zobaczyć wyjście, które zostanie zdefiniowane w funkcji `main` w pliku źródłowym `core.clj`. 

##Głębsza analiza

Kiedy już zaczniesz pracę nad swoim projektem, ważne jest, aby pamiętać o kilku rzeczach. Po pierwsze, Clojure promuje programowanie funkcyjne, co oznacza, że powinieneś unikać mutacji danych i stosować operacje na niezmienialnych danych. Po drugie, warto korzystać z biblioteki standardowej Clojure, która zawiera wiele przydatnych funkcji do manipulacji danymi.

Możesz również rozważyć użycie narzędzia CIDER, które jest popularnym środowiskiem programowania Clojure dla edytora kodu Emacs. Umożliwia to wygodne debugowanie i testowanie Twojego kodu.

##Zobacz również

- [Oficjalna strona Clojure](https://clojure.org/)
- [Dokumentacja Leiningen](https://leiningen.org/)
- [Strona CIDER](https://cider.readthedocs.io/en/latest/)