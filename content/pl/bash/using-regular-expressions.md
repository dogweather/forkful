---
title:                "Bash: Używając wyrażeń regularnych"
simple_title:         "Używając wyrażeń regularnych"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub masz zainteresowania w dziedzinie informatyki, na pewno spotkałeś się z wyrażeniami regularnymi. Są to wyrażenia, które pozwalają na efektywne i precyzyjne wyszukiwanie i manipulowanie tekstem. Dzięki nim możesz szybciej przetwarzać duże ilości danych, np. w skryptach Bash. Czy warto znać wyrażenia regularne? Oczywiście! Poznaj, dlaczego.

## Jak używać wyrażeń regularnych w Bashu

Wyrażenia regularne w Bashu są tworzone przy użyciu operatora "=~" oraz z użyciem znaku "/", który określa początek i koniec wyrażenia. Przykładowo, żeby sprawdzić czy dany tekst zawiera słowo "kot", możemy napisać:

```Bash
if [[ "To jest tekst z kotem" =~ /kot/ ]]; then
  echo "Znalazłem kota!"
fi
```

W powyższym kodzie, wyrażenie "To jest tekst z kotem" sprawdzane jest pod kątem wystąpienia wyrażenia regularnego "/kot/", a następnie wyświetlany jest komunikat "Znalazłem kota!".

Możemy także używać wyrażeń regularnych do wyodrębniania konkretnych części tekstu. Na przykład, jeśli chcemy pobrać numer telefonu z ciągu znaków, możemy to zrobić w taki sposób:

```Bash
# Przykładowy numer telefonu
text="Moje numer telefonu to 123-456-789."

if [[ "$text" =~ /([0-9]{3})-([0-9]{3})-([0-9]{3})/ ]]; then
  # Zapisujemy wyrażenie w zmiennych "area_code", "first_three" i "last_four"
  area_code="${BASH_REMATCH[1]}"
  first_three="${BASH_REMATCH[2]}"
  last_four="${BASH_REMATCH[3]}"
  echo "$area_code $first_three $last_four"
fi
```

W powyższym kodzie, wyrażenie regularne "/([0-9]{3})-([0-9]{3})-([0-9]{3})/" ujmuje trzy grupy trzycyfrowych liczb (w tym przypadku trzy części numeru telefonu) i zapisuje je w zmiennych za pomocą tablicy BASH_REMATCH.

## Pogłębione studiowanie wyrażeń regularnych

Wyrażenia regularne są bardzo potężnym narzędziem, które warto poznać dokładnie. Pozwalają one na wyszukiwanie i manipulowanie tekstem w bardzo precyzyjny sposób. W Bashu możemy wykorzystywać wiele operatorów i funkcji związanych z wyrażeniami regularnymi, takich jak np. "=~", "!" lub "basename". Warto także zapoznać się z różnymi wzorami wyrażeń regularnych, np. "[0-9]" oznacza dowolną cyfrę, "[a-z]" oznacza dowolną małą literę, a "[^a-zA-Z]" oznacza znaki, które nie są literami.

W internecie można znaleźć wiele przydatnych tutoriali i zasobów, które pomogą Ci pogłębić swoją wiedzę na temat wyrażeń regularnych. Pamiętaj, że praktyka czyni mistrza, więc nie wahaj się eksperymentować z różnymi wzorami i wykorzystywać wyrażeń regularnych w swoich projektach.

## Zobacz także

Jeśli chcesz poszerzyć swoją wiedzę na temat wyrażeń regularnych w Bashu, polecamy zapoznać się z poniższymi linkami:

- [Wyrażenia regularne w Bashu](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
- [Podstawowe wy