---
title:                "Tworzenie pliku tekstowego"
html_title:           "Go: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Ktoś może pomyśleć, że pisząc tekstowy plik w programie Go to nudna i monotonna czynność. Ale, jak się okazuje, jest to ważny element w procesie tworzenia aplikacji. Pisanie tekstowych plików pozwala na zapisywanie i odczytywanie danych w trwały sposób, co jest niezbędne w wielu projektach.

## Jak to zrobić

W Go istnieje wiele sposobów na pisanie tekstowych plików. Poniżej przedstawione są dwa z nich, które są najbardziej popularne i proste w użyciu.

### Zastosowanie pakietu "os"

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// otwarcie pliku do zapisu
	file, err := os.Create("plik.txt")
	if err != nil {
		// w razie błędu wypisze go na ekranie
		fmt.Println(err)
	}
	
	fmt.Println("Plik został utworzony.")

	// zamknięcie pliku po zapisie
	defer file.Close()

	// zapisanie tekstu do pliku
	fmt.Fprintln(file, "To jest przykładowy tekst do zapisania w pliku.")
	
	// wypisanie informacji o sukcesie na ekranie
	fmt.Println("Zapisano pomyślnie.")
}
```

Po uruchomieniu tego programu, w folderze powinien pojawić się nowy plik o nazwie "plik.txt", w którym znajdzie się zapisany tekst. Pakiet "os" pozwala nam na otwarcie pliku (Create), zapis do niego (Fprintln) oraz zamknięcie po zakończeniu (Close). Używając słowa kluczowego "defer" przed wywołaniem funkcji Close, mamy pewność, że plik zostanie zamknięty po zakończeniu wszystkich operacji.

### Zastosowanie pakietu "io/ioutil"

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	// zapisanie tekstu do zmiennej
	text := []byte("To jest przykładowy tekst do zapisania w pliku.")

	// zapisanie tej zmiennej do pliku
	err := ioutil.WriteFile("plik.txt", text, 0644)
	if err != nil {
		// w razie błędu wypisze go na ekranie
		fmt.Println(err)
	}
	
	fmt.Println("Plik został utworzony i zapisany.")
}
```

Pakiet "io/ioutil" jest bardzo użyteczny przy pisaniu krótszych i prostszych plików. W powyższym przykładzie zapisujemy tekst bezpośrednio do pliku przy użyciu funkcji WriteFile. Podanie parametru 0644 oznacza, że plik będzie dostępny dla użytkowników do odczytu i zapisu. Jest to jeden z wielu możliwych trybów dostępu do plików.

## Pogłębiona analiza

Pisanie plików jest jednym z podstawowych elementów w programowaniu i jest nieodłączną częścią wielu aplikacji. W Go dostępnych jest wiele pakietów do obsługi plików, ale te dwa przedstawione powyżej są najczęściej wykorzystywane. Ważne jest również pamiętanie o zamknięciu pliku po zakończeniu wszystkich operacji, aby nie blokować dostępu do niego dla innych procesów.

## Zobacz też

- [Dokumentacja pakietu "os"](https://golang.org/pkg/os/)
- [Dokumentacja pakietu "io/ioutil"](https://golang.org/pkg/io/ioutil/)