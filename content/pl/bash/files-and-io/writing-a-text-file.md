---
title:                "Pisanie pliku tekstowego"
aliases:
- /pl/bash/writing-a-text-file/
date:                  2024-02-03T19:27:09.738568-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie pliku tekstowego"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie pliku tekstowego w Bashu pozwala na automatyzację przechowywania danych, logowania, ustawień konfiguracyjnych i wielu innych. Jest to podstawowa umiejętność w skryptowaniu powłoki, umożliwiająca programistom zapisywanie wyników poleceń, wykonania skryptów lub danych wejściowych użytkownika do raportowania, przetwarzania lub przyszłego wykonania.

## Jak to zrobić:

Bash oferuje proste metody zapisu do pliku. Najpopularniejsze to użycie operatorów przekierowania (`>`, `>>`) oraz komendy `tee`. Oto krótkie spojrzenie na obie techniki.

Używając przekierowania, możesz zapisać dane wyjściowe bezpośrednio do pliku. Operator `>` zapisuje zawartość do pliku, zastępując go, jeśli już istnieje, podczas gdy `>>` dodaje do istniejącego pliku bez usuwania jego zawartości.

```bash
# Zapisywanie do pliku za pomocą >
echo "Witaj, Świecie!" > myfile.txt

# Dodawanie do pliku za pomocą >>
echo "To jest nowy wiersz." >> myfile.txt
```

Jeśli sprawdzisz zawartość `myfile.txt` po uruchomieniu powyższych poleceń, znajdziesz:

```
Witaj, Świecie!
To jest nowy wiersz.
```

Komenda `tee` jest przydatna, gdy chcesz zapisać do pliku i jednocześnie zobaczyć wynik na ekranie (stdout). Domyślnie `tee` zastępuje plik, ale z flagą `-a` dodaje do pliku.

```bash
# Zapisywanie i wyświetlanie za pomocą tee
echo "Cześć, znowu!" | tee myfile.txt

# Dodawanie i wyświetlanie za pomocą tee -a
echo "Dodaję kolejny wiersz." | tee -a myfile.txt
```

Po uruchomieniu tych komend, `myfile.txt` wyświetli:

```
Cześć, znowu!
Dodaję kolejny wiersz.
```

Chociaż sam Bash oferuje rozbudowane możliwości manipulowania plikami za pomocą przekierowań i poleceń takich jak `tee`, dalsza manipulacja lub bardziej złożone scenariusze mogą wymagać wywołania zewnętrznych narzędzi lub języków skryptowych (np. Awk, Sed, Python), które oferują bardziej zaawansowane funkcje przetwarzania tekstu. Jednak dla większości prostych zadań związanych z zapisem plików, powyższe metody są w pełni wystarczające i szeroko stosowane.
