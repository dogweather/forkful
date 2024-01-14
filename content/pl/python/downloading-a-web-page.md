---
title:                "Python: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych ma szerokie zastosowania w programowaniu, takie jak przetwarzanie danych do analizy, automatyzacja zadań lub web scraping do pozyskiwania informacji z internetu. Może być również przydatne w testowaniu stron internetowych lub aplikacji. Pobieranie stron internetowych może być także sposobem na praktykowanie swoich umiejętności programistycznych i uczenia się nowych technologii.

## Jak to zrobić

Pobieranie strony internetowej w Pythonie jest bardzo proste dzięki bibliotece requests. Najpierw musimy zainstalować tę bibliotekę za pomocą polecenia `pip install requests`. Następnie importujemy ją do naszego programu.

```
import requests
```

Następnie możemy użyć metody `get()` aby pobrać żądaną stronę internetową. Poniżej przedstawiony jest przykładowy kod pobierający stronę internetową Google i zapisujący ją do pliku "google.html".

```
r = requests.get('https://www.google.com/')
with open('google.html', 'wb') as f:
    f.write(r.content)
```

Po wykonaniu tego kodu, w folderze zawierającym nasz plik Python, powinien pojawić się plik "google.html". Możemy otworzyć go w przeglądarce internetowej i zobaczyć zawartość pobranej strony.

## Pogłębione zagadnienia

Pobieranie stron internetowych może wymagać dodatkowych działań w zależności od potrzeb programistycznych. Możemy na przykład wykonać parsowanie pobranej strony przy użyciu biblioteki Beautiful Soup, aby wyodrębnić potrzebne nam informacje z kodu HTML. Możemy także dodać nagłówki, zalogować się do strony lub użyć mechanizmu sesji, aby pobierać wielokrotnie dane z tej samej witryny.

Pamiętajmy jednak, że pobieranie stron internetowych zawsze powinno odbywać się zgodnie z prawem i z szacunkiem dla praw autorskich. Przed uruchomieniem kodu, upewnijmy się, że mamy zgodę na pobieranie danej strony lub wykorzystanie jej treści.

## Zobacz również

1. Dokumentacja biblioteki requests: https://docs.python-requests.org/
2. Oficjalna strona Pythona: https://www.python.org/
3. Tutorial na temat pobierania stron internetowych w Pythonie: https://realpython.com/python-web-scraping-practical-introduction/