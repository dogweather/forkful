---
aliases:
- /pl/powershell/working-with-xml/
date: 2024-01-26 04:34:39.097866-07:00
description: "Praca z XML polega na manipulowaniu i dost\u0119powi do danych zorganizowanych\
  \ w j\u0119zyku eXtensible Markup Language. Programi\u015Bci pracuj\u0105 z XML,\
  \ aby umo\u017Cliwi\u0107\u2026"
lastmod: 2024-02-18 23:08:49.852864
model: gpt-4-0125-preview
summary: "Praca z XML polega na manipulowaniu i dost\u0119powi do danych zorganizowanych\
  \ w j\u0119zyku eXtensible Markup Language. Programi\u015Bci pracuj\u0105 z XML,\
  \ aby umo\u017Cliwi\u0107\u2026"
title: Praca z XML
---

{{< edit_this_page >}}

## Co i Dlaczego?
Praca z XML polega na manipulowaniu i dostępowi do danych zorganizowanych w języku eXtensible Markup Language. Programiści pracują z XML, aby umożliwić interoperacyjność z innymi systemami lub odczytywać i zapisywać pliki konfiguracyjne, kanały danych i inne strukturyzowane dokumenty powszechne w usługach internetowych.

## Jak to zrobić:
```PowerShell
# Wczytywanie pliku XML do zmiennej
[xml]$xmlContent = Get-Content 'ścieżka\do\twego\pliku.xml'

# Dostęp do węzłów XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Tytuł: $($book.title)"
}

# Tworzenie nowego elementu XML
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Zapisywanie XML z powrotem do pliku
$xmlContent.Save('ścieżka\do\twego\zaktualizowanego\pliku.xml')
```
Przykładowe wyjście:
```
Tytuł: Programowanie PowerShell
Tytuł: Podstawy XML
```

## W głębi tematu
XML, czyli eXtensible Markup Language, istnieje od końca lat '90 i pozostaje szeroko używanym formatem dla strukturyzowanych danych. PowerShell upraszcza pracę z XML w porównaniu do tradycyjnych metod analizy; traktuje XML bezpośrednio jako obiekty, co pozwala na interakcję z elementami za pomocą znajomej notacji kropkowej.

Alternatywy dla XML to JSON, YAML czy własne formaty danych. JSON, na przykład, zyskał na popularności dzięki swojej lekkości i łatwości użycia z technologiami internetowymi. Jednak rozszerzone funkcje XML, takie jak przestrzenie nazw, schematy i przetwarzanie XSLT, często czynią go lepszym wyborem dla złożonych dokumentów lub standardów branżowych.

PowerShell używa możliwości XML Frameworka .NET do obsługi XML. Oznacza to, że chodzi nie tylko o proste operacje odczytu-zapisu; można również pracować ze schematami XML w celu walidacji, używać XPath do zapytań i stosować transformacje XSLT, wszystko to za pomocą PowerShell.

## Zobacz także
- [W3Schools samouczek XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
