---
title:                "C++: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

##_Dlaczego_

Istnieje wiele sytuacji, w których wyodrębnianie podciągów tekstu jest niezbędne w programowaniu w języku C++. Może to być potrzebne do przetwarzania danych, analizy tekstu czy też do tworzenia złożonych algorytmów. W tym artykule dowiecie się, jak wyodrębniać substryngi w prosty sposób i jak może to pomóc w waszych projektach.

##_Jak_

Przykładowy kod w języku C++ pozwoli nam lepiej zrozumieć, jak wyodrębniać substryngi. Wyobraźmy sobie, że mamy zmienną tekstową zawierającą imię i nazwisko osoby:

```C++
string fullName = "Jan Kowalski";
```

Aby wyodrębnić tylko imię, musimy zastosować metodę substr() na zmiennej fullName:

```C++
string firstName = fullName.substr(0, 3); // wynik: "Jan"
```

Funkcja substr() przyjmuje dwa parametry - pierwszy to początkowy indeks, od którego chcemy wyodrębnić podciąg, a drugi to długość podciągu. W naszym przypadku chcemy wyodrębnić pierwsze 3 znaki, więc podaliśmy indeks 0 (zaczynamy od pierwszego znaku) i długość 3.

Podobnie możemy postępować z wyodrębnianiem nazwiska:

```C++
string lastName = fullName.substr(4); // wynik: "Kowalski"
```

W tym przypadku nie podaliśmy długości podciągu, więc zostanie wyodrębniona cała reszta tekstu od podanego indeksu (4).

Po wykorzystaniu funkcji substr(), możemy dowolnie manipulować naszymi wydzielonymi podciągami, np. zamieniać je na duże litery lub sprawdzać ich długość.

##_Pogłębiona analiza_

Funkcja substr() jest bardzo przydatna, ale warto również wiedzieć o innych sposobach wyodrębniania substryngów w języku C++. Inną popularną metodą jest używanie strumienia stringstream, który pozwala na dzielenie tekstu na podciągi za pomocą delimitera (znaku, po którym tekst jest dzielony). Przykładowy kod wyglądałby tak:

```C++
string fullName = "Jan Kowalski";
stringstream ss(fullName);
string firstName, lastName;
getline(ss, firstName, ' '); // dzielimię po spacji, wynik: "Jan"
getline(ss, lastName); // wynik: "Kowalski"
```

Warto również wspomnieć o funkcji find(), która pozwala na wyszukanie indeksu danego znaku lub podciągu w tekście. Możemy jej użyć w połączeniu z substr(), aby dokładnie wyodrębnić to, czego potrzebujemy.

##_Zobacz również_

- Dokumentacja funkcji substr() w języku C++: https://www.cplusplus.com/reference/string/string/substr/
- Strumienie stringstream w języku C++: https://www.cplusplus.com/reference/sstream/stringstream/
- Funkcja find() w języku C++: https://www.cplusplus.com/reference/string/string/find/