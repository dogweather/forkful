---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Usuwanie Znaków Pasujących do Wzorca w Ruby: Krótki Przewodnik

## Co I Dlaczego?
Usuwanie znaków pasujących do wzorca to technika, która pozwala programistom precyzyjnie manipulować danymi tekstowymi. Jest to niezbędne, gdy chcemy usunąć niepotrzebne lub niechciane znaki z ciągów, np. białe znaki, znaki specjalne czy wiadomości spamowe.

## Jak to Zrobić:
Usuwanie konkretnych znaków z ciągu w Ruby jest proste. Używamy metody `delete`, podając odpowiedni wzorzec.

```Ruby
str = "Hello, World!"
str.delete("l")  # Usuń wszystkie litery 'l'
puts str
```

Wynikiem powyższego kodu będzie: `Heo, Word!`

Możemy również używać zakresów znaków.

```Ruby
str = "abc123"
str.delete("a-c")  # Usuń znaki od 'a' do 'c'
puts str
```

Wyjście: `123`

## Deep Dive
Metoda `delete` w Ruby jest jedną z podstawowych metod manipulacji produkcyjnych danych tekstowych. Wprowadzono ją w Ruby 1.1 i od tego czasu jest stale ulepszana. Alternatywą dla `delete` mogłaby być metoda `gsub`, która umożliwia również usuwanie znaków, ale z większą precyzją i możliwością używania wyrażeń regularnych. 

W implementacji, `delete` tworzy nowy ciąg bez znaków pasujących do wzorca, nie modyfikując oryginalnego ciągu. To oznacza, że `delete` jest "nie destrukcyjna". Metoda ta jest szybka i efektywna, a jej wydajność jest jednym z powodów, dla których jest tak często używana.

## Zobacz Też
- [Dokumentacja Ruby: String#delete](https://ruby-doc.org/core-2.7.1/String.html#method-i-delete)
- [Poradnik Ruby: Wyrażenia Regularne](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Stack Overflow: Jak usunąć określony znak z ciągu](https://stackoverflow.com/questions/14348480/how-to-remove-specific-character-from-string)