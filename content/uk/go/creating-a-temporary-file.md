---
title:                "Go: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є важливою частиною написання програм, які працюють з багатими данними або вимагають швидкого доступу до файлів. Це дозволяє програмі працювати з даними безпосередньо у пам'яті, що робить код більш ефективним і швидшим.

## Як

Найсвіжіша версія мови програмування Go має більш зручний і простий для використання пакет `ioutil` для створення тимчасових файлів. Для цього пакету доступні функції `TempDir` і `TempFile`, які дозволять вам створити тимчасову папку або файл в системі та доступатись до нього з вашої програми.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	tmpDir, err := ioutil.TempDir("", "example")
	if err != nil {
		panic(err)
	}
	fmt.Println("Тимчасова папка:", tmpDir)

	defer os.RemoveAll(tmpDir)

	tmpFile, err := ioutil.TempFile(tmpDir, "test")
	if err != nil {
		panic(err)
	}
	fmt.Println("Тимчасовий файл:", tmpFile.Name())

	defer os.Remove(tmpFile.Name())

	// Тут можна працювати з тимчасовим файлом

}
```

Виведення:
```
Тимчасова папка: /var/folders/3k/g1nhc24d4rvgz__9w__hm2nctzgqmz/T/example073564578
Тимчасовий файл: /var/folders/3k/g1nhc24d
```

## Глибокий пір

Існують багато інших можливостей і функцій для створення тимчасових файлів, таких як `TempFile` з пакету `os`, який дозволяє керувати правами доступу до файлу. Також, ви можете задати префікс для тимчасового файлу, що допоможе ідентифікувати його у ваших програмах.

Крім того, важливо коректно видаляти тимчасові файли та папки після закінчення роботи з ними. Це можна зробити за допомогою функції `RemoveAll` з пакету `os`, яка видалить не тільки вказану папку, але і всі файли і папки в ній.

## Дивіться також

- [Офіційна документація Go](https://golang.org/pkg/io/ioutil/#TempFile)
- [Стаття про роботу з файлами у Go](https://levelup.gitconnected.com/how-to-work-with-files-in-go-dfe9e4c181f7)
- [Стаття про створення тимчасових файлів у Go](https://www.sohamkamani.com/golang/working-with-files/)