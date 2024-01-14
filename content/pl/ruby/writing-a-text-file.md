---
title:                "Ruby: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać plik tekstowy?

Pisanie plików tekstowych może być pomocne w wielu sytuacjach. Jedną z nich jest przechowywanie danych w bezpieczny i przejrzysty sposób. Możesz również użyć pliku tekstowego do utworzenia prostych formularzy lub do przechowywania instrukcji dla swojego programu.

## Jak to zrobić?

Pisanie pliku tekstowego w języku Ruby jest bardzo proste. Wystarczy użyć metody "File.open" wraz z odpowiednimi parametrami, aby utworzyć nowy plik lub otworzyć istniejący. Następnie, przy użyciu metody "puts", możesz napisać zawartość pliku w wybranym formacie. Poniżej znajduje się przykładowy kod:

```Ruby
File.open("moj_plik.txt", "w") do |plik|
  plik.puts "To jest tekstowa zawartość mojego pliku."
end
```

Ten kod utworzy od nowa plik o nazwie "moj_plik.txt" i umieści w nim tekst "To jest tekstowa zawartość mojego pliku." W ten sam sposób możesz również wczytywać i edytować istniejące pliki tekstowe.

## Głębsze zagłębienie

Istnieje wiele różnych metod do pisania i czytania plików tekstowych w języku Ruby, takich jak "File.read", "File.write" czy "File.readlines". Możesz również użyć bloków "do/end" lub symboli specjalnych, takich jak "r" i "w", aby określić tryb operacji na pliku. W celu uzyskania dokładniejszych informacji na temat pisania plików tekstowych w Ruby, warto przeczytać dokumentację lub książki o tej tematyce. 

## Zobacz również

- Dokumentacja Ruby o pisaniu plików tekstowych: https://docs.ruby-lang.org/en/2.7.0/File.html
- "Ruby in Twenty Minutes" - dostępny za darmo poradnik o języku Ruby: https://www.ruby-lang.org/pl/documentation/quickstart/
- "Pisanie plików tekstowych w Ruby" - artykuł na stronie railsware.com: https://railsware.com/blog/important-methods-in-ruby-file-class/