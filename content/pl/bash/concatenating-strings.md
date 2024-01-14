---
title:    "Bash: Łączenie ciągów znaków"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

##Dlaczego

Łączenie łańcuchów jest częstym zadaniem w programowaniu Bash. Jest to przydatna technika, która pozwala na budowanie bardziej złożonych programów, łącząc ze sobą różne elementy tekstu. 

##Jak to zrobić

Aby połączyć dwa łańcuchy w Bash, użyj operatora `+` lub `+=`. Na przykład: 

```Bash 
name="Jan"
echo "Witaj, " + $name
```

Wynik: `Witaj, Jan`

##Głębszy wywód

Łączenie łańcuchów jest możliwe dzięki tzw. "konkatenacji", czyli po prostu łączeniu elementów w jedną całość. W Bash nie musimy używać specjalnych funkcji czy metod, po prostu dodajemy jeden łańcuch do drugiego. 

Warto również pamiętać, że połączenie dwóch liczb w Bash również jest możliwe poprzez ich "konkatenację". Jednak wynikiem takiego działania będzie zawsze również liczba, a nie nowy łańcuch. Na przykład: 

```Bash 
number1=1
number2=2
echo $number1 + $number2
```

Wynik: `3`

Oprócz tego, pamiętaj, aby w Bash używać pojedynczych cudzysłowów przy łączeniu łańcuchów, aby uniknąć niepożądanych działań. Na przykład: 

```Bash 
word1="hello"
word2="world"
echo $word1 + $word2
```

Wynik: `hello + world`

##Zobacz także

- [Dokumentacja Bash](https://www.gnu.org/software/bash/)
- [Polecenia Bash](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [Konkatenacja łańcuchów w Bash](https://linuxhint.com/concatenate_strings_bash/)