---
title:                "Ruby: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem podczas pisania kodu w Ruby możemy potrzebować usunąć pewne znaki z ciągu tekstu, które pasują do określonego wzorca. Na przykład, może chcemy usunąć wszystkie spacje lub wszystkie wystąpienia pewnego znaku. Istnieje wiele powodów, dlaczego moglibyśmy chcieć to zrobić, na przykład oczyszczanie danych, filtrowanie inputu lub przekształcanie danych wejściowych na potrzeby naszego programu.

## Jak to zrobić

Do usunięcia znaków pasujących do wzorca w Ruby możemy wykorzystać metody `gsub` lub `delete`. Pierwsza z nich pozwala nam na podanie wzorca, który chcemy usunąć, oraz na określenie czym chcemy go zastąpić. Jeśli chcemy po prostu usunąć znaki, możemy przekazać pusty ciąg znaków `""` jako drugi argument. 

```Ruby
"Hello, World!".gsub(/[aeiou]/, "")  #=> "Hll, Wrld!"
```

W powyższym przykładzie wykorzystujemy wyrażenie regularne, aby wskazać, które znaki chcemy usunąć. W przypadku gdy chcemy usunąć wszystkie wystąpienia konkretnego znaku, możemy po prostu przekazać go jako pierwszy argument.

```Ruby
"Hello, World!".delete("l")  #=> "Heo, Word!"
```

Obie te metody zwrócą nam nowy ciąg znaków bez wybranych przez nas znaków.

## Głębsze zagadnienia

Podczas wykorzystywania metody `gsub` do usunięcia znaków pasujących do wzorca, możemy dodatkowo wykorzystać dodatek `i`, aby ignorować wielkość liter (case-insensitive). Możemy także wykorzystać zapis skrócony, aby przy użyciu wyrażenia regularnego usunąć wszystkie dowolne znaki oprócz wybranych, np.:

```Ruby
"123abc".gsub(/[^0-9]/, "")  #=> "123"
```

W przypadku, gdy chcemy usunąć znaki z dołączonych na końcu lub początku ciągu znaków, zaleca się wykorzystanie metody `strip`, która automatycznie usunie białe znaki i zwróci nam przetworzony ciąg.

## Zobacz także
- [Dokumentacja Ruby: Metoda `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Dokumentacja Ruby: Metoda `delete`](https://ruby-doc.org/core-2.7.1/String.html#method-i-delete)
- [Przewodnik po wyrażeniach regularnych w Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)