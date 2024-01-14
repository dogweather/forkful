---
title:                "Bash: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas tworzenia skryptów Bash lub edycji istniejących plików tekstowych, istnieje potrzeba usunięcia określonych znaków lub wzorców z tekstu. W tym przypadku można skorzystać z funkcji usunięcia znaków w Bash. Może to przyśpieszyć pracę oraz usunąć niechciane znaki, co jest szczególnie przydatne w przypadku dużej ilości tekstu, który wymaga modyfikacji.

## Jak to zrobić

Aby usunąć znaki odpowiadające danemu wzorcowi w Bash, można użyć polecenie `tr` (transliterate). Poniżej przedstawiono prosty przykład wykorzystania tego polecenia:

```
Bash
# Przykładowy tekst do modyfikacji
text="To jest przykładowy tekst do modyfikacji"

# Usuwanie znaków 'a' i 'o'
new_text=$(echo $text | tr -d "ao")

# Wyświetlenie zmodyfikowanego tekstu
echo $new_text

# Output: T jest przykldwy tekst d mdifikcji
```

W tym przypadku wykorzystano polecenie `echo` do przekazania zmiennej `text` do polecenia `tr` w celu usunięcia znaków 'a' i 'o'. Następnie wynik został przypisany do zmiennej `new_text` i wyświetlony za pomocą polecenia `echo`.

## Deep Dive

Polecenie `tr` jest bardzo potężne i pozwala na usunięcie wielu różnych znaków lub wzorców za pomocą jednej komendy. Aby usunąć więcej niż jeden znak, można po prostu przekazać je jako argument do polecenia `tr`. Na przykład, aby usunąć wszystkie znaki interpunkcyjne ze zmiennej `text` można użyć następującej komendy:

```
Bash
new_text=$(echo $text | tr -d '[:punct:]')
```

Ponadto, istnieje możliwość podania zakresu znaków do usunięcia z użyciem polecenia `tr`. Na przykład, aby usunąć wszystkie cyfry ze zmiennej `text`, można zastosować polecenie:

```
Bash
new_text=$(echo $text | tr -d '0-9')
```

Powyższe przykłady dotyczą usunięcia znaków z pojedynczej zmiennej, ale można również użyć polecenia `tr` do modyfikowania plików tekstowych. W tym przypadku zamiast przekazywać zmienną jako argument, podaje się nazwę pliku. Na przykład, jeśli chcemy usunąć wszystkie samogłoski z pliku `tekst.txt`, możemy użyć następującej komendy:

```
Bash
tr -d 'aeiou' < tekst.txt
```

Spowoduje to wyświetlenie zmodyfikowanego tekstu bez samogłosek na ekranie. Aby zapisać wynik do nowego pliku, można użyć symbolu `>` oraz nazwy nowego pliku, na przykład `tr -d 'aeiou' < tekst.txt > nowy_tekst.txt`.

## Zobacz również

- Dokumentacja polecenia `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Przydatne polecenia Bash: https://linuxize.com/post/bash-commands/
- Tutoriale Bash: https://www.shell-tips.com/bash/