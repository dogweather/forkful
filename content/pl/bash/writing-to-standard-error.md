---
title:                "Bash: Pisanie do standardowego błędu"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć czytelniku! Jeśli jesteś programistą lub uczysz się programowania, z pewnością słyszałeś o standardowym błędzie i zapisywaniu do niego. W tym wpisie postaram się przybliżyć Ci ten temat i wyjaśnić, dlaczego warto zaprzyjaźnić się z tym narzędziem.

## Jak to zrobić

Do zapisywania do standardowego błędu używa się polecenia ```echo```, a następnie przekierowuje się wyjście do standardowego błędu przy użyciu symbolu ```>&2```. Na przykład:

```Bash
echo "To jest błąd" >&2
```

Powoduje wypisanie tekstu "To jest błąd" do standardowego błędu. Możesz również użyć standardowego wyjścia, aby przekazać treść błędu do standardowego błędu, jak w przykładzie poniżej:

```Bash
ls nieistniejący_katalog 1>&2
```

Powyższe polecenie spowoduje, że w przypadku, gdy katalog nie istnieje, zostanie wyświetlony komunikat błędu, a nie "kolejne linie kodu".

## Głębsza analiza

Przekierowywanie do standardowego błędu jest przydatne w wielu sytuacjach. Przede wszystkim, jest to jedna z podstawowych technik obsługi błędów w skryptach Shell. Pozwala też na wygodne wypisywanie komunikatów błędów użytkownikowi podczas uruchamiania programów lub skryptów. Dodatkowo, można użyć specjalnych warunków i pętli do przekazywania informacji o błędach, co ułatwia debugowanie kodu.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o zapisywaniu do standardowego błędu w Bash, polecam przeczytać te artykuły:

- [https://linuxhint.com/bash_error_stderr/](https://linuxhint.com/bash_error_stderr/)
- [https://www.redhat.com/sysadmin/using-stderr](https://www.redhat.com/sysadmin/using-stderr)