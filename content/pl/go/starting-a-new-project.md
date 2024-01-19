---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Rozpoczęcie nowego projektu to stworzenie zupełnie nowego kodu, który możemy kształtować do naszych własnych potrzeb. Robimy to, aby rozwiązać unikalne problemy, dla których nie ma gotowych rozwiązań lub potrzebny jest bardziej spersonalizowany podejście.

## Jak to zrobić:

Zaczynamy od stworzenia nowego katalogu dla naszego projektu:

```Go
mkdir myproject
cd myproject
```

Następnie musimy zainicjować moduł, niech nazwa modułu będzie tak jak nazwa naszego projektu:

```Go
go mod init myproject
```

Teraz możemy stworzyć nasz pierwszy plik go:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
```

Uruchomienie tego kodu da nam następujący wynik:

```Go
Hello, world!
```

## Głębsze spojrzenie:

Rozpoczynanie nowego projektu jest zawsze ekscytującym wydarzeniem, ale również od czasu do czasu może nas napotkać wiele wyzwań - historycznie rzecz biorąc, były to na ogół te same rodzaje problemów, które spotykamy dzisiaj. Na szczęście, współczesne narzędzia, takie jak Go, oferują wiele funkcji, które pomagają w zarządzaniu projektem.

Alternatywą dla Go jest wiele, w tym popularne języki takie jak Python, Java i Rust. Każdy z nich ma swoje mocne strony, ale Go wyróżnia się prostotą i skutecznością, szczególnie dla większych systemów.

Szczegółowa implementacja rozpoczęcia nowego projektu w Go obejmuje wiele kroków. Przeważnie zaczynamy od inicjacji modułu, co stwarza 'go.mod', a to jest istotnym elementem, który umożliwia narzędziu Go śledzenie zależności.

## Zobacz również

Można znaleźć więcej informacji i wskazówek na oficjalnej stronie Go: https://golang.org/doc/
Jest tam cała sekcja poświęcona zarządzaniu projektem, która może być pomocna: https://golang.org/doc/code.html#Organization

Gorąco polecam również przeczytać artykuł Boba Nystroma o wyzwaniach, które można spotkać podczas pisania nowego kodu: http://journal.stuffwithstuff.com/2014/12/08/the-challenges-of-starting-a-new-codebase/