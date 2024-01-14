---
title:    "Ruby: Usuwanie znaków pasujących do wzorca."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś mógłby chcieć usuwać znaki pasujące do pewnego wzoru w swoim kodzie? Przyczyny mogą być różne - może chcemy oczyścić dane z niepożądanych znaków, lub zamienić pewne ciągi znaków na inne.

## Jak to zrobić

```Ruby
# Tworzymy przykładowy ciąg znaków
string = "To jest przykladowy ciag znakow!!!"

#Korzystając z metody `gsub` i wyrażenia regularnego, możemy usunąć wszystkie znaki nie będące literami lub spacjami
new_string = string.gsub(/[^a-zA-Z\s]/,"")

puts new_string #=> "To jest przykladowy ciag znakow"

# W powyższym przykładzie, wyrażenie regularne /[^a-zA-Z\s]/ oznacza "wszystkie znaki, które nie są literami albo spacjami".

```

## Dogłębna analiza

Wykorzystanie wyrażeń regularnych pozwala nam na precyzyjne określenie wzorców, których szukamy w ciągu znaków. Możemy na przykład wykorzystać różne znaki specjalne, które ułatwią nam wyszukanie dokładnie tych znaków, które chcemy usunąć. Przykładowo, zamiast `[a-zA-Z\s]` możemy użyć `[0-9]`, aby usunąć wszystkie cyfry z naszego ciągu znaków.

## Zobacz także

- Dokumentacja Ruby o wyrażeniach regularnych: https://ruby-doc.org/core-2.7.1/Regexp.html 
- Przykładowy kod usuwający znaki specjalne: https://www.rubyguides.com/2015/05/working-with-strings-in-ruby/#remove-all-punctuation
- Wyrażenia regularne w praktyce: https://www.rubyguides.com/2015/06/ruby-regex/