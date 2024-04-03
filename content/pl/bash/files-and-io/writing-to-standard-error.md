---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:28.417520-07:00
description: "Jak to zrobi\u0107: W Bashu u\u017Cywasz `>&2` do przekierowania wyj\u015B\
  cia do stderr. Oto podstawowy przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.601775-06:00'
model: gpt-4-0125-preview
summary: "W Bashu u\u017Cywasz `>&2` do przekierowania wyj\u015Bcia do stderr."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

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
