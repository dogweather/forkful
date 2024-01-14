---
title:    "Go: Tworzenie pliku tekstowego"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie plików tekstowych jest ważne

Pisanie plików tekstowych jest ważnym elementem procesu tworzenia oprogramowania. Pozwala ono na przechowywanie i przetwarzanie danych, a także na komunikację między różnymi systemami. W dzisiejszych czasach, gdy programiści często współpracują z innymi osobami lub urządzeniami, umiejętność pisania plików tekstowych jest niezbędna.

## Jak to zrobić

Poniżej znajdują się przykładowe kody w języku Go, które pozwolą Ci na naukę pisania plików tekstowych. Przykładowe dane wyświetlane będą w tzw. blokach kodu ```Go ...```, co pozwoli Ci lepiej zrozumieć sposób działania kodu.

```Go
package main 

import (
	"fmt"
	"io/ioutil"
)
 
func main() {
	// Tworzenie pliku i uzupełnienie go danymi
	f := []byte("Hello world!")
	err := ioutil.WriteFile("sample.txt", f, 0644)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("Plik tekstowy został utworzony pomyślnie.")
	}

	// Odczytywanie danych z pliku
	data, err := ioutil.ReadFile("sample.txt")
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(string(data))
	}
}
```

Po uruchomieniu tego kodu, w katalogu, w którym znajduje się plik programu, zostanie utworzony plik tekstowy o nazwie "sample.txt" i jego zawartość zostanie wyświetlona w konsoli.

## Głębszy zanurz się w temat

Umiem pisać pliki tekstowe jest ważnym aspektem programowania, dlatego warto poświęcić więcej czasu na zgłębienie tego tematu. Poniżej znajdują się kilka przydatnych linków, które pozwolą Ci pogłębić swoją wiedzę na ten temat:

- [Dokumentacja języka Go](https://golang.org/)
- [Poradnik tworzenia plików w języku Go](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go)
- [Blog Go In Action - praktyczne porady dla programistów języka Go](https://blog.golang.org/)

## Zobacz również

- [Inne przydatne informacje o języku Go](https://www.django-cms.org/pl/blog/y/lekcja-3-wprowadzenie-do-go/)
- [Kurs języka Go dla początkujących](https://www.udemy.com/course/the-complete-golang-bootcamp/)
- [Społeczność języka Go - forum i grupy dyskusyjne](https://forum.golangbridge.org/)