---
title:    "Bash: Sprawdzanie istnienia katalogu"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, że ważną częścią wykonywania zadań jest upewnienie się, czy dany katalog istnieje. Nie tylko zabezpiecza to nas przed błędami w kodzie, ale także pomaga w zarządzaniu plikami i folderami. W tym artykule dowiesz się, jak sprawdzić istnienie katalogu w Bashu.

## Jak to zrobić

Aby sprawdzić, czy katalog istnieje w Bashu, użyjemy polecenia `test` z opcją `-d`, która sprawdza, czy dany argument jest katalogiem. Spójrzmy na przykładowy kod:

```Bash
#!/bin/bash

if test -d "folder"; then
    echo "Katalog istnieje"
else
    echo "Katalog nie istnieje"
fi
```

W powyższym przykładzie, jeśli katalog `"folder"` istnieje, zostanie wydrukowany napis "Katalog istnieje". W przeciwnym razie, wydrukowany zostanie napis "Katalog nie istnieje".

Możemy także użyć skróconego zapisu z opcją `-d` polecenia `test`:

```Bash
#!/bin/bash

if [ -d "folder" ]; then
    echo "Katalog istnieje"
else
    echo "Katalog nie istnieje"
fi
```

Możemy również wykorzystać tę wiedzę w bardziej skomplikowanych skryptach, np. aby kopiować pliki tylko wtedy, gdy dany katalog istnieje:

```Bash
#!/bin/bash

if [ -d "folder" ]; then
    cp file.txt folder 
    echo "Plik został skopiowany"
else
    echo "Katalog nie istnieje, nie można skopiować pliku"
fi
```

## Deep Dive

W Bashu istnieje również wiele innych opcji polecenia `test` do sprawdzania różnych warunków, w tym m.in. czy plik istnieje (`-e`), czy jest pusty (`-s`) czy czytelny (`-r`). W przykładach powyżej skupiliśmy się jedynie na opcji `-d` do sprawdzania istnienia katalogu.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o poleceniu `test` i różnych opcjach, możesz zapoznać się z oficjalną dokumentacją [Basha](https://www.gnu.org/software/bash/manual/bash.html). Jeśli interesują Cię inne sposoby sprawdzania istnienia katalogu w Bashu, warto zajrzeć na [Stack Overflow](https://stackoverflow.com/), gdzie można znaleźć wiele przydatnych porad od innych programistów.