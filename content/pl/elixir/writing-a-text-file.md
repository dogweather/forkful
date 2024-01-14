---
title:    "Elixir: Tworzenie pliku tekstowego"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią wielu programistycznych zadań. W przypadku języka Elixir, pisanie plików tekstowych jest proste i niezawodne, co sprawia, że jest to ważna umiejętność dla każdego programisty.

## Jak to zrobić

Aby napisać plik tekstowy w Elixir, możemy użyć funkcji `File.write/2` lub `File.write!/2`. Przykładowy kod wykorzystujący pierwszą funkcję wyglądałby następująco:

```elixir
File.write("plik.txt", "To jest przykładowy tekst")
```

Za pomocą funkcji `File.write!/2` możemy dodatkowo zdefiniować tryb w jakim ma być tworzony plik, na przykład `:append` lub `:utf8`:

```elixir
File.write!("plik.txt", "Nowy tekst", [:append])
```

Jeśli chcielibyśmy uzyskać dostęp do zawartości pliku, możemy wykonac funkcję `File.read/1`:

```elixir
File.read("plik.txt")
```

W tym przykładzie, funkcja zwróci w konsoli zawartość pliku `plik.txt`.

## Głębsze zagadnienia

Podstawowy zapis i odczyt plików tekstowych w Elixir jest bardzo prosty, jednak jeśli chcemy poznać bardziej zaawansowane techniki, możemy wykorzystać funkcję `IO.binwrite/2`, która pozwala na zapisanie danych w postaci binarnej. Oprócz tego, warto zwrócić uwagę na funkcję `File.stream!/3`, która umożliwia odczyt pliku w postaci strumienia.

## Zobacz również

Jeśli jesteś zainteresowany/zainteresowana poznaniem więcej na temat obsługi plików w Elixir, możesz skorzystać z poniższych linków:

- Dokumentacja Elixir na temat tworzenia plików: [https://elixir-lang.org/getting-started/file-operations.html](https://elixir-lang.org/getting-started/file-operations.html)
- Przewodnik na temat pisania plików w Elixir: [https://dockyard.com/blog/2016/05/19/writing-files-with-elixir](https://dockyard.com/blog/2016/05/19/writing-files-with-elixir)