---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Bash: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usunięcie znaków pasujących do wzorca jest częstym zadaniem w programowaniu w Bashu, które często jest wykonywane jako część skryptów lub w celu automatyzacji pewnych zadań. Pozwala to na szybkie i skuteczne przetwarzanie tekstu, co jest niezbędne w wielu scenariuszach.

## Jak to zrobić

Przy użyciu polecenia `sed` i wykorzystując wyrażenia regularne, możemy łatwo usunąć znaki pasujące do określonego wzorca z tekstu. Oto prosty przykład:

```Bash
sed 's/[a-zA-Z]//g' plik.txt
```

Powyższa komenda usunie wszystkie litery z podanego pliku tekstowego. W tym przypadku wykorzystujemy wyrażenie regularne `[a-zA-Z]`, które oznacza wszystkie litery zarówno małe, jak i duże. Możemy również użyć innych wyrażeń regularnych w celu dopasowania odpowiednich znaków do usunięcia.

## Deep Dive

Usunięcie znaków pasujących do wzorca jest możliwe dzięki wykorzystaniu polecenia `sed`, które jest potężnym narzędziem do przetwarzania tekstu w Bashu. Dzięki wyrażeniom regularnym możemy precyzyjnie określić, które znaki mają być usunięte. Możemy również używać innych opcji, takich jak `g` (global) w powyższym przykładzie, aby usunąć wszystkie pasujące znaki z całego tekstu, a nie tylko pierwsze wystąpienie.

Inną możliwością jest wykorzystanie polecenia `tr`, które również służy do translacji lub usunięcia znaków. W tym przypadku możemy użyć opcji `-d` (delete), aby usunąć wszystkie pasujące znaki z tekstu z pominięciem wyrażeń regularnych.

## Zobacz także

- [Dokumentacja Bash na stronie oficjalnej](https://www.gnu.org/software/bash/)
- [Poradnik od Basha do AWK](https://www.gnu.org/software/bash/)
- [Podstawy wyrażeń regularnych w Bashu](https://www.gnu.org/software/bash/)