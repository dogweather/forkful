---
title:    "Go: Rozpoczynanie nowego projektu"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedykolwiek, dlaczego warto zacząć nowy projekt w technologii Go? W tym artykule podpowiemy Ci dlaczego warto i jak zacząć programowanie w Go.

## Jak to zrobić

Aby zacząć kodowanie w Go, musisz najpierw zainstalować odpowiednie narzędzia. Poniżej przedstawiamy przykład instalacji dla systemu Windows:

```Go
go get -u golang.org/dl/go1.15.2.windows-amd64.msi
```

Po zainstalowaniu Go, możesz utworzyć nowy projekt za pomocą polecenia:

```Go
go mod init [nazwa projektu]
```

Następnie możesz utworzyć plik z kodem i uruchomić go za pomocą polecenia:

```Go
go run [nazwa pliku]
```

W przykładzie poniżej tworzymy prosty program, który wypisuje powitanie na konsoli:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Cześć! Witaj w świecie Go!")
}
```

Po uruchomieniu programu, powinno pojawić się na ekranie wyjście:

Cześć! Witaj w świecie Go!

## Dogłębna analiza

Teraz, gdy już wiesz jak zacząć kodowanie w Go, możesz zagłębić się w szczegóły tworzenia nowego projektu. Pierwszym krokiem będzie zapoznanie się z dokumentacją języka Go, która dostępna jest na oficjalnej stronie: https://golang.org/doc/.

Następnie musisz zapoznać się z podstawowymi konceptami języka, takimi jak typy danych, pętle, funkcje czy struktury. Możesz to zrobić poprzez dokładne przejrzenie przykładów dostępnych w dokumentacji.

Kolejnym krokiem będzie nauka korzystania z narzędzi i środowiska programistycznego. Warto zapoznać się z edytorem kodu, który posiada wsparcie dla Go, na przykład Visual Studio Code.

Podczas pisania kodu w Go ważne jest również stosowanie dobrych praktyk programistycznych i dbałość o wydajność. W sieci można znaleźć wiele artykułów i poradników dotyczących tych tematów.

## Zobacz też

- Dokumentacja języka Go: https://golang.org/doc/
- Oficjalny kurs na stronie Go: https://tour.golang.org/welcome/1
- Poradnik dla początkujących w Go: https://golangbot.com/learn-golang-series/
- Poradnik dotyczący wydajności w Go: https://stackimpact.com/docs/performance-go/
- Zbiór przydatnych narzędzi dla programistów Go: https://github.com/golang/go/wiki/Tools