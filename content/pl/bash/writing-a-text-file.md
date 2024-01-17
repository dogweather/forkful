---
title:                "Pisanie pliku tekstowego"
html_title:           "Bash: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Co i dlaczego? 
Pisanie plików tekstowych jest jedną z podstawowych czynności programistów. Polega ona na tworzeniu i zapisywaniu tekstu w formacie, który użytkownik może odczytać i zrozumieć. Programiści korzystają z tej funkcji, aby przechowywać dane lub wyniki swojego kodu w łatwy do przetworzenia sposób. 

# Jak to zrobić? 
Możesz utworzyć nowy plik tekstowy w Bash za pomocą poleceń ```touch``` lub ```echo```. Na przykład, wpisanie ```touch nowy_plik.txt``` stworzy pusty plik o nazwie "nowy_plik.txt". Możesz także dodać tekst do istniejącego pliku, używając polecenia ```echo```. Na przykład ```echo "Hello World!" > nowy_plik.txt``` doda zdanie "Hello World!" do pliku "nowy_plik.txt". 
 Możesz również wykorzystać edytor tekstu, taki jak Vim lub Nano, aby ręcznie pisać newytekstowe. 

# Przekroczmy horyzonty 
Pisanie plików tekstowych jest podstawową umiejętnością w programowaniu i używane jest we wszystkich językach programowania. Jednakże, zawsze można wykorzystać inne narzędzia, takie jak bazy danych lub arkusze kalkulacyjne, aby przechowywać i przetwarzać dane. Implementacja zależy od systemu operacyjnego, ale w zasadzie każdy system ma wbudowane polecenia do tworzenia i zapisywania plików tekstowych.

# Zobacz także 
Dla innych sposobów na manipulowanie plikami tekstowymi w Bash, zobacz ten [poradnik](https://www.tutorialspoint.com/unix/unix-io-redirections.htm) lub post udostępniony na [Stack Overflow](https://stackoverflow.com/questions/12321492/how-to-write-data-to-a-text-file-using-bash).