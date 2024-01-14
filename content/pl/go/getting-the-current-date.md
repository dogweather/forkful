---
title:                "Go: Pobieranie aktualnej daty"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Zawsze zastanawiałeś się jak uzyskać aktualną datę oraz godzinę w języku programowania Go? Ta krótka instrukcja wyjaśni Ci jak to zrobić krok po kroku.

## Jak to zrobić?

Uzyskanie aktualnej daty w języku Go jest bardzo proste. Musimy skorzystać z funkcji "time.Now()", która zwraca nam aktualną datę i czas w postaci obiektu "Time". Wystarczy wywołać tę funkcję i przypisać jej wynik do zmiennej.

```Go
currentTime := time.Now()
fmt.Println(currentTime)
```

Powyższy kod wyświetli aktualną datę i godzinę w formacie:

```
2021-10-30 13:45:00 +0100 CET
```

Jeśli chcemy wyświetlić tylko datę bez godziny, możemy skorzystać z metody "Format" i odpowiedniego parametru. Poniżej przykład wyświetlający samą datę:

```Go
fmt.Println(currentTime.Format("2006-01-02"))
```

Output:

```
2021-10-30
```

## Głębsze zanurzenie

Funkcja "time.Now()" korzysta z lokalnego systemowego zegara, więc jeśli zmienimy strefę czasową, wynik również ulegnie zmianie. Aby uniknąć tego problemu, możemy wykorzystać metodę "In" i podać jako parametr strefę czasową, w której chcemy uzyskać aktualną datę.

```Go
currentTime := time.Now().In(time.UTC)
fmt.Println(currentTime)
```

Output:

```
2021-10-30 12:45:00 +0000 UTC
```

Możemy również wyświetlić dzień tygodnia oraz godzinę w odpowiednim formacie. W poniższym przykładzie wyłączamy część związana z dniem tygodnia i formatujemy tylko godzinę.

```Go
fmt.Println(currentTime.Format("15:04:05"))
```

Output:

```
13:45:00
```

## Zobacz również

- Dokumentacja funkcji "time.Now()" w języku Go: https://pkg.go.dev/time#Now
- Przykładowe kody korzystające z funkcji "time.Now()": https://gist.github.com/marpiech/d38b07d53cfa812792b3e5d9ca7947f3