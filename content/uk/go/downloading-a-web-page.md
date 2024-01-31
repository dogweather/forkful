---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:12.445082-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Завантаження веб-сторінки – це процес отримання HTML-коду сторінки з сервера. Програмісти роблять це, щоб зчитати дані, автоматизувати процеси тестування веб-інтерфейсів, або створювати скрапери контенту.

## How to: (Як це зробити:)
```Go
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func DownloadWebPage(url string) error {
	// Створення HTTP запиту
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	// Відкриття файлу для запису
	outFile, err := os.Create("output.html")
	if err != nil {
		return err
	}
	defer outFile.Close()

	// Копіювання вмісту веб-сторінки у файл
	_, err = io.Copy(outFile, resp.Body)
	return err
}

func main() {
	url := "http://example.com"
	err := DownloadWebPage(url)
	if err != nil {
		fmt.Println("Error downloading page:", err)
		return
	}
	fmt.Println("Web page downloaded successfully")
}
```
**Sample output:**
```
Web page downloaded successfully
```

## Deep Dive (Поглиблений розгляд)
В історії інтернету завантаження веб-сторінок змінювалось від простих HTTP запитів до роботи з складними JavaScript-програмами. Go, зі своїм стандартним пакетом `net/http`, пропонує легкий спосіб завантаження статичного контенту; динамічний контент може потребувати додаткових бібліотек, таких як "chromedp" чи "rod" для роботи з браузерами. Важливими аспектами є обробка відповідей сервера, належна робота з запитами/відповідями та правильне закриття ресурсів за допомогою `defer`.

## See Also (Дивіться також)
- Офіційна документація Go 'net/http' пакету: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- "chromedp" для роботи з браузерами: [https://github.com/chromedp/chromedp](https://github.com/chromedp/chromedp)
- "rod" бібліотека для автоматизації браузерів: [https://github.com/go-rod/rod](https://github.com/go-rod/rod)
- Go by Example, працюючи з HTTP: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)
