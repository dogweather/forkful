---
title:                "Bash: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, co to są argumenty wiersza poleceń i dlaczego są one ważne w programowaniu? Dla wszystkich tych, którzy są zainteresowani tworzeniem własnych skryptów i automatyzacją, znajomość obsługi argumentów wiersza poleceń jest niezbędna. Dowiedz się, dlaczego warto poznać ten temat w poniższym artykule.

## Jak to zrobić

Obsługiwanie argumentów wiersza poleceń jest dosyć proste w Bash. Wystarczy użyć zmiennych specjalnych, takich jak "$1" dla pierwszego argumentu, "$2" dla drugiego, itd. Oto przykładowy skrypt:

```Bash
#!/bin/bash

echo "Twój pierwszy argument to: $1"
echo "Twój drugi argument to: $2"
```

Po uruchomieniu skryptu w ten sposób:

`./skrypt.sh pierwszy_argument drugi_argument`

Otrzymamy taki output:

```
Twój pierwszy argument to: pierwszy_argument
Twój drugi argument to: drugi_argument
```

Zauważ, że dopóki podamy odpowiednią ilość argumentów, możemy wykorzystać je do dowolnych zadań w skrypcie.

## Głębsza analiza

W rzeczywistości, Bash oferuje dużo więcej opcji niż tylko te zmienne specjalne, gdy chodzi o obsługę argumentów wiersza poleceń. Na przykład możemy użyć pętli do przetworzenia wszystkich argumentów:

```Bash
#!/bin/bash

for argument in "$@"
do
    echo "Pobrany argument to: $argument"
done
```

Możemy również dodać warunki do naszego skryptu, aby obsłużyć tylko pewne argumenty lub wyświetlić wiadomość, jeśli podano niepoprawną liczbę argumentów.

Możliwości jest wiele, więc warto poświęcić trochę czasu na zgłębienie tego tematu i dostosowanie go do swoich potrzeb.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o obsłudze argumentów wiersza poleceń w Bash, możesz zapoznać się z poniższymi artykułami:

- [Dokumentacja Bash na temat argumentów wiersza poleceń](https://bash.cyberciti.biz/guide/$1)
- [Przydatne przykłady korzystania z argumentów wiersza poleceń w Bash](https://www.baeldung.com/linux/use-command-line-arguments-in-bash-script)
- [Tutorial na temat przetwarzania argumentów wiersza poleceń w Bash](https://www.tutorialspoint.com/unix_commands/bash_argv.htm)