---
title:    "Bash: Usuwanie znaków pasujących do wzoru"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się potrzebować usunięcia wszystkich znaków dopasowujących do danego wzorca? Nie martw się, nie jesteś w tym sam. Dlatego w tym poście pokażemy, jak usunąć znaki dopasowujące do wzorca za pomocą języka Bash.

## Jak to zrobić?

Sprawdźmy najpierw jaki jest składnią usuwania znaków dopasowujących do wzorca w Bash. W prosty sposób możemy to zrobić za pomocą znaku odwróconego ukośnika `\` oraz gwiazdki `*`. Na przykład, jeśli chcemy usunąć wszystkie znaki "a" z ciągu znaków, możemy użyć poniższej komendy:

```Bash
echo "Jakieś słowo z wieloma literami a" | tr -d 'a'
```

W wyniku otrzymamy:

```
Jkieś słowo z wielom lterimi
```

W przypadku, gdy chcemy usunąć znaki dopasowujące do konkretnego wzorca, możemy użyć polecenia `grep` w połączeniu z potokiem danych. Na przykład, jeśli chcemy usunąć wszystkie liczby z tekstu, możemy użyć poniższej komendy:

```Bash
echo "Czy 1 chcesz 2 nauczyć się 3 kodować?" | grep -oE '[0-9]*' | tr -d '\n'
```

W wyniku otrzymamy:

```
Czy chcesz nauczyć się kodować?
```

Opcja `-o` w poleceniu `grep` pozwala na wyświetlenie tylko dopasowanych fragmentów tekstu, a `-E` umożliwia użycie wyrażeń regularnych.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej na temat usuwania znaków dopasowujących do wzorca w języku Bash, możesz przejrzeć dokumentację dla polecenia `tr` lub `grep`. Istnieje wiele opcji i możliwości, które mogą być przydatne w różnych sytuacjach. Polecamy również zapoznać się z wyrażeniami regularnymi, ponieważ są one bardzo przydatne w usuwaniu znaków dopasowujących do określonego wzorca.

## Zobacz także
- [Dokumentacja dla polecenia `tr`](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Dokumentacja dla polecenia `grep`](https://www.gnu.org/software/grep/manual/grep.html)
- [Wyrażenia regularne - poradnik w języku polskim](https://kursjs.pl/kurs/regex/regular-expression-introduction.php)