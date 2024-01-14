---
title:    "Ruby: Tworzenie pliku tekstowego"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest ważnym elementem dla każdego programisty. Pozwala na przechowywanie i przetwarzanie danych w czytelnej i uporządkowanej formie. Dodatkowo, pliki tekstowe są powszechnie wykorzystywane w wielu programach i narzędziach, dlatego warto poznać ich zasady tworzenia.

## Jak

W Ruby istnieją kilka sposobów na pisanie plików tekstowych. Zaprezentuję Wam jeden z nich, korzystając z wbudowanych metod w języku.

```Ruby
# Utworzenie pliku i zapisanie w nim tekstu
File.write("plik.txt", "To jest moja przykładowa treść")

# Odczytanie tekstu z pliku
tekst = File.read("plik.txt")
puts tekst # => To jest moja przykładowa treść

# Dodanie tekstu do istniejącego pliku
File.open("plik.txt", "a") do |f|
  f.puts "\nTo jest kolejny przykładowy tekst"
end

# Wczytanie poszczególnych linii tekstu z pliku
File.readlines("plik.txt").each do |linia|
  puts linia.chomp # usunięcie znaku nowej linii
end
# Output:
# To jest moja przykładowa treść
# To jest kolejny przykładowy tekst
```

## Deep Dive

Pisząc pliki tekstowe w Ruby, warto pamiętać o kilku ważnych elementach. Po pierwsze, należy dobrać odpowiedni tryb otwarcia pliku - "w" (zapis do pliku, nadpisywanie istniejącej zawartości) lub "a" (dodawanie do pliku, bez nadpisywania). Warto też pamiętać o dopisywaniu znaku nowej linii (```\n```) po każdej linii tekstu oraz o zamknięciu pliku po jego użyciu.

## Zobacz również

- [Ruby Dokumentacja - Pliki](https://ruby-doc.org/core-2.6/File.html)
- [Wprowadzenie do zapisu plików tekstowych w Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Kurs Ruby - Pliki](https://www.learnrubyonline.org/pl/03-fundamentals-files)