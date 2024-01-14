---
title:                "Ruby: Sprawdzanie istnienia katalogu"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego istnieje potrzeba sprawdzania, czy katalog istnieje w kodzie Ruby? Często w naszych projektach potrzebujemy wykonać różne operacje na plikach, a jedną z nich jest tworzenie, edycja lub usuwanie plików w konkretnym katalogu. Bez sprawdzenia, czy dany katalog istnieje, program może zakończyć się błędem, co może wpłynąć na działanie naszej aplikacji. Dlatego ważne jest, aby zawsze upewnić się, czy katalog, który chcemy użyć, istnieje przed wykonaniem operacji na plikach.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje w Ruby jest bardzo proste i wymaga tylko kilku linii kodu. Wykorzystajmy metodę `exist?` z modułu `File`, aby sprawdzić istnienie katalogu o nazwie "dane".

```Ruby
if File.exist?("dane")
  # kod, który zostanie wykonany, jeśli katalog istnieje
  puts "Katalog istnieje."
else
  # kod, który zostanie wykonany, jeśli katalog nie istnieje
  puts "Katalog nie istnieje."
end
```
Przykładowy output:

```
Katalog nie istnieje.
```

Dodatkowo, jeśli chcemy również upewnić się, że dany katalog jest katalogiem (a nie np. plikiem), możemy wykorzystać metodę `directory?` z modułu `File`.

```Ruby
if File.directory?("dane")
  # kod, który zostanie wykonany, jeśli katalog istnieje
  puts "To jest katalog."
else
  # kod, który zostanie wykonany, jeśli katalog nie istnieje lub jest plikiem
  puts "To nie jest katalog."
end
```
Przykładowy output:

```
To jest katalog.
```

## Pogłębione informacje

Podczas sprawdzania, czy dany katalog istnieje, Ruby przechodzi przez wszystkie foldery w ścieżce i sprawdza ich istnienie. Jeśli wszystkie foldery istnieją, metoda `exist?` zwróci wartość `true`. Jeśli choć jeden z folderów nie istnieje, metoda zwróci wartość `false`. Możemy więc wykorzystać tę informację do bardziej zaawansowanych operacji, na przykład utworzenia brakujących folderów.

## Zobacz także

- [Moduł File w dokumentacji Ruby](https://ruby-doc.org/core/File.html)
- [Metoda `exist?` w dokumentacji Ruby](https://ruby-doc.org/core/File.html#method-c-exist-3F)
- [Metoda `directory?` w dokumentacji Ruby](https://ruby-doc.org/core/File.html#method-c-directory-3F)