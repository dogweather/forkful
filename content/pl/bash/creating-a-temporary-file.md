---
title:                "Bash: Tworzenie pliku tymczasowego"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w Bashu jest nie tylko szalenie użyteczne, ale też bardzo ciekawe. Jedną z najważniejszych umiejętności, które muszą posiadać programiści Bash, jest tworzenie plików tymczasowych. Dlaczego? Cóż, mamy dla Ciebie kilka powodów.

Po pierwsze, pliki tymczasowe są bardzo przydatne w procesie tworzenia i testowania kodu. Pozwalają one na przechowywanie tymczasowych danych, które mogą być potrzebne w trakcie działania programu. Dzięki temu nie musimy obciążać naszego systemu operacyjnego, a nasz kod jest bardziej niezawodny i odporny na błędy.

Po drugie, tworzenie plików tymczasowych jest ważne w kontekście bezpieczeństwa danych. Wyobraź sobie, że pracujesz nad programem, który przechowuje wrażliwe informacje, takie jak hasła. Jeśli wszystko byłoby przechowywane w jednym pliku, to w przypadku jego wycieku, wszystkie te informacje mogłyby zostać ujawnione. Dzięki plikom tymczasowym, możemy rozdzielić nasze dane i zmniejszyć ryzyko wycieku informacji.

## Jak stworzyć plik tymczasowy

Tworzenie pliku tymczasowego w Bashu jest bardzo proste. Wystarczy użyć polecenia `mktemp`, które automatycznie tworzy plik tymczasowy z unikalną nazwą. Spróbujmy tego przykładu:

```Bash
file=$(mktemp)
echo "To jest zawartość mojego pliku tymczasowego" > $file
cat $file
```

W tym przypadku, zmienna `file` przechowuje nazwę pliku tymczasowego, a następnie wypisujemy do niego pewną zawartość. Oczywiście, zawartość ta może być dowolna w naszym programie.

## Głębszy wgląd

Tworzenie plików tymczasowych w Bashu jest jeszcze bardziej zaawansowane niż to, co pokazaliśmy powyżej. W rzeczywistości, możemy ustawić wiele opcji, takich jak modyfikowanie nazwy pliku, ustalanie innej lokalizacji czy dostosowywanie prefiksu i sufiksu. Aby poznać pełną listę opcji, możesz sprawdzić dokumentację polecenia `mktemp`.

Należy również pamiętać o usuwaniu plików tymczasowych po zakończeniu działania naszego programu. Możemy to zrobić ręcznie, używając polecenia `rm`, lub automatycznie, ustalając odpowiednie opcje podczas tworzenia pliku tymczasowego.

## Zobacz również

- [Dokumentacja `mktemp`](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Kurs Bash na Codecademy](https://www.codecademy.com/learn/learn-the-command-line)