---
title:                "Python: HTML verarbeiten"
simple_title:         "HTML verarbeiten"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Parser für HTML sind nützliche Tools, um Daten von einer Webseite zu extrahieren und sie für die Verarbeitung in anderen Programmen zugänglich zu machen. Dies kann besonders hilfreich sein, wenn man bestimmte Informationen von einer Website benötigt, aber keinen direkten Zugriff auf eine API hat oder wenn man die Daten in einem bestimmten Format benötigt, das nicht bereits von der Website bereitgestellt wird.

## Wie

Um mit dem Parsen von HTML in Python zu beginnen, müssen wir zuerst das `bs4` Paket, auch bekannt als Beautiful Soup, importieren. Dies ist eine populäre und benutzerfreundliche Bibliothek für das Arbeiten mit HTML und XML Daten. Wir können es auf zwei Arten importieren:

```Python
from bs4 import BeautifulSoup
```

oder

```Python
import bs4
```

Als nächstes müssen wir unsere HTML Daten erhalten, sei es durch das Öffnen einer lokalen Datei oder durch das Senden einer GET-Anfrage an eine Website. Wir werden die Funktion `requests.get` nutzen, um eine GET-Anfrage an die gewünschte URL zu senden und die Antwort in einer Variablen zu speichern:

```Python
url = "https://example.com"
response = requests.get(url)
```

Wir können nun die `BeautifulSoup` Funktion verwenden, um unsere HTML Daten aus der Antwort in ein Soup-Objekt zu konvertieren:

```Python
soup = BeautifulSoup(response.content, 'html.parser')
```

Schließlich können wir mithilfe der verschiedenen Funktionen von Beautiful Soup bestimmte Tags oder Klassen in unserem HTML finden und die entsprechenden Daten extrahieren. Ein Beispiel dafür ist die Verwendung von `find_all`, um alle Links auf der Seite zu finden und sie dann in eine Liste zu speichern:

```Python
all_links = soup.find_all('a')

for link in all_links:
    print(link.get('href'))
```

Die obige Methode kann je nach den spezifischen Bedürfnissen angepasst werden. Weitere Informationen zu den verschiedenen Möglichkeiten der Verwendung von Beautiful Soup finden Sie in der Dokumentation.

## Deep Dive

Um ein besseres Verständnis dafür zu bekommen, wie HTML Parsing funktioniert, können wir einen Blick auf die grundlegenden Konzepte dahinter werfen. HTML besteht aus verschiedenen Tags, die den Inhalt der Seite strukturieren und formatieren. Beautiful Soup analysiert unsere HTML Daten und identifiziert diese Tags und ihre Hierarchie. Dadurch wird es uns ermöglicht, gezielt auf bestimmte Teile der HTML Struktur zuzugreifen.

Um die HTML Tags und ihre Struktur besser zu verstehen, können wir uns das DOM (Document Object Model) ansehen. Dies ist eine Baumstruktur, die die Beziehung zwischen den HTML Elementen auf der Seite darstellt. Mit Beautiful Soup können wir auf diese Baumstruktur zugreifen und Informationen von den verschiedenen Elementen extrahieren.

## Siehe auch

- [Beautiful Soup Dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Eine ausführliche Anleitung zum Parsen von HTML in Python](https://www.dataquest.io/blog/web-scraping-python-using-beautiful-soup/)