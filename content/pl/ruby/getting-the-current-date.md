---
title:    "Ruby: Uzyskiwanie bieżącej daty"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego?

W dzisiejszych czasach programowanie jest bardzo powszechne i ważne w różnych dziedzinach. Może to być z powodów zawodowych, hobbystycznych lub edukacyjnych. Jednym z podstawowych zadań programowania jest pracowanie z datami i czasem. W tym wpisie dowiesz się, jak w prosty sposób uzyskać obecny dzień w języku Ruby.

## Jak to zrobić?

Aby uzyskać obecny dzień, musimy skorzystać z metody "Time.now". Następnie możemy wykorzystać metodę "strftime" do sformatowania daty w odpowiedni sposób. Oto przykładowy kod w języku Ruby:

```Ruby
dzisiaj = Time.now
puts dzisiaj.strftime("%d-%m-%Y")
```

W powyższym przykładzie, wyświetli się bieżąca data w formacie "DD-MM-RRRR".

Możemy także wyświetlić inną informację o bieżącym czasie, na przykład godzinę:

```Ruby
puts dzisiaj.strftime("%H:%M")
```

Wynik tego kodu będzie wyglądał w następujący sposób: "14:30". Jak widać, możliwości są bardzo szerokie i możesz sformatować datę tak, jak chcesz.

## Głębszy zanurzenie

Metoda "strftime" jest bardzo przydatna, ale aby ją wykorzystać, musimy znać odpowiednie formaty. Oto kilka najczęściej wykorzystywanych formatów:

- %d - dzień miesiąca (np. 05)
- %m - miesiąc (np. 05)
- %Y - rok (np. 2020)
- %H - godzina w formacie 24-godzinnym (np. 14)
- %M - minuta (np. 30)
- %S - sekunda (np. 45)
- %A - dzień tygodnia (np. Poniedziałek)
- %B - miesiąc w pełnej nazwie (np. Maj)
- %w - dzień tygodnia w formacie numerycznym (0 dla niedzieli)

Możesz także wykorzystać symbole specjalne, takie jak "/", "-", ":" do sformatowania daty w sposób, który jest dla ciebie czytelny.

## Zobacz także

- Dokumentacja języka Ruby: https://ruby-doc.org/core-2.7.1/Time.html
- Poradnik dla początkujących: https://www.rubyguides.com/2015/05/working-with-dates-ruby/
- Przykładowy kod z wykorzystaniem innych metod: https://www.rubyguides.com/2015/05/ruby-time/