---
title:                "Bash: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w Bashu jest bardzo popularne wśród programistów i administratorów systemów. Jednym z podstawowych zadań w Bashu jest manipulacja ciągami znaków, a znajomość długości tych ciągów jest niezbędna do wykonywania różnych operacji. W tym artykule dowiesz się, jak znaleźć długość ciągu znaków w Bashu, a także dlaczego jest to ważne.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w Bashu, możesz użyć wbudowanego polecenia ```len```. Przyjmie ono jeden argument - ciąg znaków - i zwróci jego długość. Instrukcja wygląda następująco:

```
len="To jest przykładowy ciąg znaków"
echo ${#len}
```

Wyjście z tego polecenia będzie wynosić 29, ponieważ to jest długość całego ciągu znaków.

Możesz również wykorzystać to polecenie w celu znalezienia długości ciągu znajdującego się w zmiennej. Na przykład:

```
name="Kasia"
echo ${#name}
```

Wyjście wyniesie 5, co oznacza długość ciągu "Kasia".

Pamiętaj, że to polecenie zwraca długość ciągu znaków, a nie indeks ostatniego znaku. Przykładowo, jeśli masz ciąg znaków "Hello World", długość wyniesie 11, a nie 10 (ponieważ polecenie zlicza też spację).

## Deep Dive

Podczas przetwarzania danych w Bashu, często będziesz musiał określać długość ciągu znaków w celu wykonania odpowiednich operacji. Jednym z przykładów jest walidacja danych wprowadzonych przez użytkownika - jeśli wprowadzony ciąg jest za krótki lub za długi, możesz wyświetlić odpowiedni komunikat.

Możesz również wykorzystać długość ciągu do wyciągania konkretnej części z tekstu. Na przykład, jeśli masz listę nazwisk w formacie "Nazwisko, Imię", możesz użyć długości ciągu i poleceń takich jak ```cut``` lub ```awk``` do wyciągnięcia tylko imienia lub nazwiska.

Warto także pamiętać, że Bash oferuje inne przydatne polecenia do manipulacji ciągami znaków, takie jak ```substr``` czy ```expr```. Warto zapoznać się z nimi i wybrać odpowiednie narzędzie do danego problemu.

## Zobacz też

* [Intro to Bash Strings](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-5.html)
* [String Manipulation in Bash](https://linuxconfig.org/string-manipulation-in-bash)
* [Bash String Manipulation Examples](https://linuxhint.com/bash-string-manipulation-examples/)

Dzięki temu artykułowi powinieneś/-aś mieć już wyraźniejszy obraz tego, jak znaleźć długość ciągu znaków w Bashu i dlaczego jest to przydatne. Pamiętaj, że ciągi znaków są podstawowymi elementami w wielu programach i warto zapoznać się z różnymi metodami ich przetwarzania.