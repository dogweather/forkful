---
title:    "Bash: Generowanie losowych liczb"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią programowania w Bashu. Wiele zadań wymaga użycia losowych liczb, więc umiejętność generowania ich w celu rozwiązania problemów może być nieoceniona.

## Jak to zrobić

W Bashu istnieje kilka sposobów na wygenerowanie losowego ciągu liczb. Jednym z najprostszych sposobów jest użycie polecenia `shuf`, które losuje elementy z pliku lub przekazanego jako argument listy. Na przykład, jeśli chcesz wylosować liczbę od 1 do 10, możesz użyć polecenia:

```Bash
shuf -i 1-10 -n 1
```

Będzie to wyglądać następująco w terminalu:

```Bash
$ shuf -i 1-10 -n 1
5
```

Innym sposobem jest użycie funkcji `RANDOM`, która generuje losową liczbę całkowitą od 0 do 32767. Możesz ją wykorzystać w pętli for lub while, aby wygenerować wiele losowych liczb. Na przykład, aby wylosować 10 liczb, możesz napisać:

```Bash
for ((i=1; i<=10; i++))
do
  echo $RANDOM
done
```

Oto przykładowy wynik, który może pojawić się w terminalu:

```Bash
$ for ((i=1; i<=10; i++)) ; do echo $RANDOM ; done
23005
748
22202
21557
5630
23264
29641
7303
18814
16761
```

Możesz także skorzystać z funkcji `seq`, która tworzy listę liczb z określonego przedziału, a następnie wykorzystać polecenie `shuf` do wylosowania z niej. Na przykład:

```Bash
shuf -n 1 <(seq 1 50)
```

Wygeneruje losową liczbę od 1 do 50.

## Pogłębione informacje

Generowanie losowych liczb w Bashu jest realizowane poprzez wykorzystanie generatora liczb pseudolosowych, czyli algorytmu, który na podstawie ustalonego ziarna generuje ciąg liczb wyglądających na losowe. W Bashu używa się generatora o nazwie LCG (Linear Congruential Generator), który jest również wykorzystywany w innych językach programowania.

Istnieje wiele sposobów na ulepszenie generowania liczb losowych w Bashu, na przykład poprzez wykorzystanie zewnętrznych bibliotek lub algorytmów. Jednak wyżej wymienione metody są wystarczające w większości przypadków i nie wymagają żadnych dodatkowych narzędzi.

## Zobacz także

- [Poradnik: Jak generować losowe liczby w Bashu] (https://www.cyberciti.biz/faq/unix-linux-generating-random-password/) 
- [Dokumentacja polecenia `shuf`] (https://man7.org/linux/man-pages/man1/shuf.1.html) 
- [Lista dostępnych generatorów liczb pseudolosowych w Bashu] (https://linux.die.net/man/1/shuf)