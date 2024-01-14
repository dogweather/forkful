---
title:    "Python: Ekstrakcja podciągów"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z tekstem w języku Python, to na pewno spotkałeś się z sytuacją, w której potrzebowałeś wyciągnąć określone części tekstu. Może chciałeś uzyskać tylko nazwisko z pełnego imienia i nazwiska, lub wyodrębnić numer telefonu z dłuższego ciągu znaków. W takich przypadkach korzystanie z funkcji do wycinania podsekwencji - nazywanych też substrings - jest niezbędne.

## Jak to zrobić

Wyciąganie substrings w języku Python jest proste dzięki wbudowanej funkcji *slice()* i metodzie *replace()* dla stringów. W poniższych przykładach wyjaśnimy jak użyć tych funkcji w różnych sytuacjach.

### Przykład 1: Wyciąganie nazwiska

```Python
imie_i_nazwisko = "Jan Kowalski"
nazwisko = imie_i_nazwisko[-8:]
print(nazwisko)
```

Output: Kowalski

### Przykład 2: Wyciąganie numeru telefonu

```Python
numer_telefonu = "(+48) 123-456-789"
czysty_numer = numer_telefonu.replace("(", "").replace(")", "").replace("-", "")
print(czysty_numer)
```

Output: +48123456789

## Pełne zanurzenie

Funkcja *slice()* wyprowadza podciag znaków ze stringa na podstawie podanego indeksu początkowego i końcowego. Istnieją również opcjonalne argumenty dla kroku oraz indeksu początkowego i końcowego dla kroku. Jeśli nie poda się indeksu początkowego, domyślnie jest nim 0, a jeśli nie poda się końcowego, domyślnie jest nim długość stringa.

Metoda *replace()* zastępuje wszystkie wystąpienia podanego tekstu w stringu innym podanym tekstem. Jeśli nie chcemy wprowadzać dodatkowych znaków, możemy podać pusty string jako drugi argument. Poniższy przykład ilustruje jak użyć obu tych funkcji do wycięcia określonego fragmentu z tekstu.

```Python
tekst = "Dzisiaj jest piękny dzień"
wyciety_tekst = tekst[9:22].replace("i", "y")
print(wyciety_tekst)
```

Output: piękne dzień

## Zobacz też

Jeśli chcesz dowiedzieć się więcej na temat wycinania podsekwencji w języku Python, warto zapoznać się z oficjalną dokumentacją oraz innymi przydatnymi artykułami na ten temat:

- [Oficjalna dokumentacja Pythona o wycinaniu podsekwencji](https://docs.python.org/3/library/functions.html#slice)
- [Przewodnik po funkcji slice w języku Python](https://realpython.com/python-slices/)
- [Poradnik na temat pracy ze stringami w języku Python](https://www.programiz.com/python-programming/string)

Dzięki umiejętności wycinania substrings, będziesz mógł sprawnie manipulować tekstem w swoim kodzie. Zapraszamy do eksperymentowania i stosowania tych technik w swoich projektach.