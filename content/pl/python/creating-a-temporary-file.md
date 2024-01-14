---
title:                "Python: Tworzenie tymczasowego pliku"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest niezbędnym elementem programowania w Pythonie. Pozwala to na przechowywanie danych w celu późniejszego wykorzystania oraz zapewnia czystość i porządek w kodzie. Bez tego elementu, programy mogą stać się nieporęczne i trudne do zrozumienia. Dlatego warto poznać możliwości tworzenia tymczasowych plików i umiejętnie je wykorzystywać.

## Jak to zrobić

Tworzenie tymczasowych plików w Pythonie jest bardzo proste. Wystarczy użyć modułu "tempfile" oraz wywołać funkcję "mktemp()" aby wygenerować unikalną nazwę dla naszego pliku. Poniżej przedstawiony jest przykładowy kod, który tworzy tymczasowy plik, zapisuje do niego pewne dane oraz odczytuje je ponownie:

```Python
import tempfile

# Tworzymy tymczasowy plik i zapisujemy do niego dane
with tempfile.TemporaryFile(mode='w+') as temp_file:
    temp_file.write("To jest przykładowy tekst")

    # Przewijamy na początek pliku i odczytujemy z niego dane
    temp_file.seek(0)
    data = temp_file.read()
    print(data)
```

Powyższy kod wykorzystuje funkcję "TemporaryFile()" z parametrem "mode='w+'", który oznacza, że plik będzie otwarty w trybie zapisu i odczytu. Warto zwrócić uwagę na użycie bloku "with" - dzięki temu nie musimy pamiętać o ręcznym zamykaniu pliku, ponieważ zostanie on zamknięty automatycznie po wyjściu z bloku.

Wynikiem powyższego kodu będzie wyświetlenie na konsoli tekstu "To jest przykładowy tekst". W ten sposób możemy łatwo przekazywać dane między różnymi fragmentami naszego programu.

## Głębokie zanurzenie

Moduł "tempfile" oferuje dużo większe możliwości niż tylko tworzenie tymczasowych plików. Przykładowo można korzystać z funkcji "mkstemp()" lub "NamedTemporaryFile()", które pozwalają na bardziej szczegółową kontrolę nad tworzeniem plików tymczasowych. Dodatkowo, moduł ten oferuje również mechanizmy do tworzenia czytelnych nazw dla plików, obsługi wyjątków i wiele innych funkcji.

Warto zapoznać się z dokumentacją modułu "tempfile" aby poznać wszystkie dostępne funkcje oraz możliwości ich używania.

## Zobacz także

- Dokumentacja modułu "tempfile": https://docs.python.org/3/library/tempfile.html 
- Ciekawa prezentacja na temat tworzenia tymczasowych plików w Pythonie: https://www.youtube.com/watch?v=MHs2ziEZpuw 
- Przykładowe zastosowania tworzenia tymczasowych plików w codziennej pracy programisty: https://realpython.com/python-tempfile/