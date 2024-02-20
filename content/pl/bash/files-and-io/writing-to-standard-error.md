---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:28.417520-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) w Bashu polega na\
  \ kierowaniu komunikat\xF3w b\u0142\u0119d\xF3w lub wszelkich wa\u017Cnych wyj\u015B\
  \u0107 diagnostycznych oddzielnie od\u2026"
lastmod: 2024-02-19 22:04:54.741915
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) w Bashu polega na kierowaniu\
  \ komunikat\xF3w b\u0142\u0119d\xF3w lub wszelkich wa\u017Cnych wyj\u015B\u0107\
  \ diagnostycznych oddzielnie od\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) w Bashu polega na kierowaniu komunikatów błędów lub wszelkich ważnych wyjść diagnostycznych oddzielnie od standardowego wyjścia (stdout). Programiści robią to, aby łatwo identyfikować, rejestrować lub nawet ignorować komunikaty o błędach, co pomaga w debugowaniu i procesach logowania.

## Jak to zrobić:
W Bashu używasz `>&2` do przekierowania wyjścia do stderr. Oto podstawowy przykład:

```bash
echo "To jest zwykła wiadomość"
echo "To jest wiadomość o błędzie" >&2
```

Uruchomienie tego skryptu wyświetli obie wiadomości w konsoli, ale jeśli je przekierujesz, możesz oddzielić stdout od stderr. Na przykład:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` będzie zawierał `"To jest zwykła wiadomość"`, podczas gdy `error.txt` przechwyci `"To jest wiadomość o błędzie"`.

Dla praktycznego przypadku użycia, rozważ skrypt, który przetwarza pliki i zgłasza błąd, jeśli plik nie istnieje:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename nie istnieje!" >&2
    exit 1
else
    echo "Przetwarzanie $filename"
fi
```

Przykładowe wyjście bezpośrednio w konsoli, gdy `example.txt` nie istnieje:

```
example.txt nie istnieje!
```

Nie ma bezpośrednich bibliotek firm trzecich w Bashu do obsługi stderr, ponieważ przekierowanie jest natywnie obsługiwane i ogólnie wystarczające. Jednakże dla złożonych aplikacji można włączyć ramy logowania lub zewnętrzne narzędzia do logowania, takie jak `syslog` czy `log4bash`, aby skuteczniej zarządzać zarówno stdout, jak i stderr.
