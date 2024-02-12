---
title:                "Pobieranie strony internetowej"
aliases:
- /pl/python/downloading-a-web-page.md
date:                  2024-01-20T17:44:33.849526-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
"## Co i dlaczego?"

Pobieranie strony internetowej to proces zapisywania jej zawartości na dysk. Programiści robią to, by przetwarzać dane, automatyzować zadania lub archiwizować informacje.

## How to:
"## Jak to zrobić:"

Do pobierania stron użyjemy biblioteki `requests`. Oto przykład, jak to zrobić:

```Python
import requests

url = 'http://example.com'
response = requests.get(url)

if response.ok:
    html_content = response.text
    print(html_content[:100])  # Wyświetlamy pierwsze 100 znaków
else:
    print("Nie udało się pobrać strony.")
```

Output:

```Python
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
<...
```

## Deep Dive:
"## Wnikliwa analiza:"

Kiedy internet stawał się popularny, zaczęto pobierać strony, aby działać na danych offline. Alternatywy to `wget` i `curl`, ale są to narzędzia wiersza poleceń. W Pythonie, przed `requests`, popularna była biblioteka `urllib`, która jest bardziej niskopoziomowa.

`requests` jest wygodna i czytelna - ważne dla utrzymania kodu. Dużą zaletą jest obsługa sesji i ciasteczek. Programiści mogą dodatkowo wykorzystać takie opcje jak parametryzowanie zapytań czy obsługa timeoutu.

## See Also:
"## Zobacz również:"

- Dokumentacja `requests`: https://requests.readthedocs.io/
- Porównanie `requests` i `urllib`: https://realpython.com/requests-vs-urllib/
- Tutorial `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Informacje o `curl`: https://curl.se/docs/manual.html
