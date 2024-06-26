---
date: 2024-01-26 03:47:48.517790-07:00
description: "Jak to zrobi\u0107: Bash nie jest wyposa\u017Cony w wbudowany debugger,\
  \ tak jak niekt\xF3re inne j\u0119zyki, ale mo\u017Cna u\u017Cy\u0107 wbudowanych\
  \ polece\u0144 takich jak `set -x`, aby\u2026"
lastmod: '2024-03-13T22:44:35.589547-06:00'
model: gpt-4-0125-preview
summary: "Bash nie jest wyposa\u017Cony w wbudowany debugger, tak jak niekt\xF3re\
  \ inne j\u0119zyki, ale mo\u017Cna u\u017Cy\u0107 wbudowanych polece\u0144 takich\
  \ jak `set -x`, aby \u015Bledzi\u0107, co si\u0119 dzieje."
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Bash nie jest wyposażony w wbudowany debugger, tak jak niektóre inne języki, ale można użyć wbudowanych poleceń takich jak `set -x`, aby śledzić, co się dzieje. Lub, jako ulepszenie, jest dostępny `bashdb`, właściwy debugger umożliwiający krok po kroku przeglądać kod. Oto mały podgląd:

```Bash
# Użycie set -x do debugowania
set -x
echo "Rozpoczęcie debugowania"
my_var="Witaj, świecie debugowania!"
echo $my_var
set +x

# Użycie bashdb
# Zainstaluj bashdb za pomocą menedżera pakietów, np. apt, yum, brew.
# Debuguj skrypt o nazwie my_script.sh:
bashdb my_script.sh
```

Wynik podczas uruchamiania z `set -x`:
```Bash
+ echo 'Rozpoczęcie debugowania'
Rozpoczęcie debugowania
+ my_var='Witaj, świecie debugowania!'
+ echo 'Witaj, świecie debugowania!'
Witaj, świecie debugowania!
+ set +x
```

## Głębsze zanurzenie
Historycznie, debugowanie skryptów Bash oznaczało zasypywanie kodu instrukcjami `echo`. Ale potem pojawiło się `set -x`, dające nam wgląd w wykonanie w czasie rzeczywistym bez ręcznych wydruków. Dla tych, którzy pragną większej kontroli, pojawił się debugger `bashdb`, inspirowany debuggerem gdb dla C/C++.

Jeśli chodzi o alternatywy, poza poleceniami `set` (`-x`, `-v`, `-e`), inne opcje obejmują przekierowanie wyjścia do pliku do analizy lub użycie zewnętrznych narzędzi takich jak ShellCheck do statycznej analizy.

Pod względem implementacji, `set -x` jest łatwe; to natywna opcja Bash, która drukuje polecenia i ich argumenty w momencie wykonywania. Z drugiej strony, `bashdb` umożliwia przeglądanie kodu krok po kroku, ustawianie punktów przerwania i ocenianie wyrażeń - rzeczy, które dają ci szansę w walce z bardziej nieuchwytnymi błędami.

## Zobacz także
- Projekt Debuggera Bash: http://bashdb.sourceforge.net/
- "Pro Bash Programming" autorstwa Chrisa Johnsona i Jayanta Varmy dla zaawansowanego skryptowania.
- ShellCheck do statycznej analizy: https://www.shellcheck.net/
