---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowego pliku to proces tworzenia pliku, który jest używany tylko na krótki okres czasu. Programiści robią to, aby przechowywać dane, które są potrzebne tylko tymczasowo lub aby testować funkcje, które manipulują plikami.

## Jak to zrobić:

W Pythonie, możemy wykorzystać moduł `tempfile` do tworzenia tymczasowych plików. Oto przykład:

```python
import tempfile

tmp = tempfile.TemporaryFile()

tmp.write(b'To jest tymczasowy plik')
tmp.seek(0)

print(tmp.read())
tmp.close()
```

Gdy uruchomimy powyższy kod, zobaczymy coś takiego:

```
b'To jest tymczasowy plik'
```

## Deep Dive

Tworzenie tymczasowych plików ma swoje korzenie w dawnych dniach programowania, kiedy pamięć była cenna i trudna do uzyskania. Zamiast zużywać cenną pamięć na dane, które byłyby potrzebne tylko przez krótki czas, programiści zaczęli używać tymczasowych plików.

Jedną z alternatyw dla tworzenia tymczasowych plików jest użycie strumieni danych, ale nie zawsze są one odpowiednie, na przykład kiedy musisz wielokrotnie odczytywać te same dane.

Co do szczegółów implementacji, w Pythonie moduł `tempfile` używa specjalnych funkcji systemu operacyjnego do tworzenia bezpiecznych i unikalnych tymczasowych plików. Efektem jest to, że nawet przy wielowątkowości, nie musisz martwić się o konflikty nazw plików.

## Zobacz też 

- [Python Tempfile – Create Temporary File and Directory](https://www.pythonguides.com/python-tempfile/)

- [Python Docs - 15.2. tempfile — Generate temporary files and directories](https://docs.python.org/3/library/tempfile.html)