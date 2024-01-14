---
title:    "Python: Tworzenie pliku tekstowego"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest powszechną i niezbędną czynnością w programowaniu Python. Pozwala na zapisywanie i przechowywanie informacji w czytelnej formie, co jest szczególnie przydatne w przypadku dużych ilości danych. Może również służyć jako sposób na komunikację z innymi programistami lub użytkownikami.

## Jak to zrobić

Aby utworzyć plik tekstowy za pomocą Pythona, należy użyć wbudowanej funkcji "open()", która pozwala na otwarcie i tworzenie plików w różnych trybach. Na przykład, aby utworzyć nowy plik tekstowy i zapisać w nim kilka linii tekstu, można użyć następującego kodu:

```python
plik = open("nowy_plik.txt", "w")
plik.write("To jest pierwsza linijka tekstu.\n")
plik.write("A to jest druga linijka tekstu.")
plik.close()
```

W powyższym przykładzie, funkcja "open()" jest użyta z dwoma parametrami: nazwą pliku ("nowy_plik.txt") oraz trybem "w" (od ang. write), który informuje Pythona, że chcemy otworzyć plik w celu pisania. Następnie, przy użyciu metody "write()" na obiekcie pliku, zapisujemy dwie linijki tekstu, dodając znak nowej linii ("\n”) po pierwszej linijce. Na koniec, należy zamknąć plik za pomocą metody "close()" aby upewnić się, że wszystkie dane zostały zapisane.

## Głębsze spojrzenie

Istnieje kilka ważnych rzeczy do zapamiętania podczas pisania plików tekstowych w Pythonie:

- Podczas używania funkcji "write()", pamiętaj o dodawaniu znaku nowej linii ("\n") na końcu każdej linijki tekstu, jeśli chcesz, aby kolejne linijki były zapisane w osobnych linijkach w pliku.

- Możesz również użyć różnych trybów otwarcia pliku, w zależności od swoich potrzeb. Na przykład, tryby "r" (od ang. read) oraz "a" (od ang. append) pozwalają na odczytywanie i dopisywanie danych do istniejącego pliku.

- Podczas używania funkcji "open()", upewnij się, że podałeś poprawną ścieżkę do swojego pliku, w przeciwnym wypadku Python może nie być w stanie go odnaleźć.

## Zobacz także

- [Dokumentacja Pythona o otwieraniu plików](https://docs.python.org/3/library/functions.html#open)
- [Samouczek Pythona o pisaniu do plików](https://www.learnpython.org/pl/Wczytywanie_i_pisanie_do_plikow)
- [Tutorial na realpython.com o operacjach na plikach w Pythonie](https://realpython.com/read-write-files-python/)