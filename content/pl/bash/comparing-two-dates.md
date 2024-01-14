---
title:    "Bash: Porównywanie dwóch dat"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego porównywanie dwóch dat jest ważne?

Porównywanie dat może być bardzo przydatne w wielu różnych sytuacjach. Na przykład, jeśli pamiętasz, że dwa wydarzenia miały miejsce w różnych dniach, ale nie jesteś pewien, który nastąpił wcześniej, możesz użyć porównania dat, aby to ustalić. Możesz również wykorzystać porównanie dat do sprawdzenia, czy określona data jest wcześniejsza lub późniejsza niż inna, co może być ważne w przypadku zadawania terminów lub planowania wydarzeń.

## Jak porównać dwie daty w Bash?

Porównywanie dat w Bash jest całkiem proste dzięki wbudowanym funkcjom systemowym. W poniższym przykładzie pokazane jest, jak użyć porównania dat w Bash:

```bash
#!/bin/bash

date1="2020-09-10"
date2="2021-01-01"

if [ "$date1" \< "$date2" ]
then
  echo "$date1 jest wcześniejsze niż $date2."
elif [ "$date1" \> "$date2" ]
then
  echo "$date1 jest późniejsze niż $date2."
else
  echo "$date1 i $date2 są tym samym dniem."
fi
```

Kod ten wyświetli wiadomość, która porówna datę1 z datą2 i określi, która jest wcześniejsza. Możesz również zmienić symbole porównania (np. \<, \>, <=, >=) w zależności od twoich potrzeb.

## Głębszy wgląd w porównywanie dwóch dat

W Bash, daty są przechowywane w formacie Epoch, czyli liczba sekund od 1 stycznia 1970 roku. Dzięki temu wbudowana funkcja **date** jest w stanie porównywać daty, ponieważ przekształca one datę do formatu Epoch przed wykonaniem porównania.

Możesz również użyć innego formatu daty, takiego jak DD/MM/YYYY, ale musisz najpierw przekształcić go do formatu Epoch przy użyciu innego polecenia, takiego jak **date -d**, aby porównanie działało poprawnie.

Możesz również porównywać daty z wykorzystaniem operatorów logicznych, takich jak AND (**&&**) i OR (**||**), aby bardziej skomplikowane porównania dwóch dat.

## Zobacz również

- [Porównywanie dat w Bash - Dokumentacja Linux](https://www.linux.org/docs/man1/date.html)
- [Porównywanie dat w Bash - Tutorial na YouTube](https://www.youtube.com/watch?v=3vLaVYc5cpo)
- [Skrypty Bash dla początkujących - eBook](https://ebooks.worldofbooks.com/beginners-bash-scripts/)