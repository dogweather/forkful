---
title:                "Fish Shell: Pobranie bieżącej daty"
simple_title:         "Pobranie bieżącej daty"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym wpisie omówimy jedną z podstawowych rzeczy, jaką każdy programista powinien znać: pobieranie bieżącej daty. Bez względu na to, czy piszesz skrypty w systemie Linux czy kodujesz aplikacje na platformie MacOS, dostęp do bieżącej daty jest ważnym elementem w wielu zastosowaniach. Sprawdźmy więc, jak w prosty sposób uzyskać bieżącą datę za pomocą języka Fish Shell.

## Jak To Zrobić

```Fish Shell
echo (date +"%d-%m-%Y")
```

Powyższa komenda pokaże nam bieżącą datę w formacie dzień-miesiąc-rok. Możemy też użyć innych opcji formatowania, np. ```%A``` dla wyświetlenia dnia tygodnia lub ```%H:%M:%S``` dla godziny, minuty i sekundy. Aby poznać wszystkie dostępne opcje, warto zajrzeć do dokumentacji języka Fish Shell.

Możemy także zapisywać bieżącą datę do zmiennej i wykorzystywać ją w dalszej części skryptu:

```Fish Shell
set current_date (date +"%d-%m-%Y")
echo "Dziś mamy: $current_date!"
```

Powyższy kod zapisze bieżącą datę do zmiennej i wyświetli ją wraz z informacją "Dziś mamy: ".

## W Głąb Tematu

Aby lepiej zrozumieć, jak działa pobieranie bieżącej daty w Fish Shell, warto wspomnieć o podstawowych pojęciach, które są zaangażowane w ten proces. Po pierwsze, wykorzystujemy komendę ```date```, która jest odpowiedzialna za wyświetlanie lub modyfikację daty. Następnie wykorzystujemy opcję ```+%d-%m-%Y```, która określa, w jakim formacie ma zostać wyświetlona data. Pozostałe opcje są równie proste do zrozumienia i można z łatwością dostosować je do swoich potrzeb.

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o języku Fish Shell i jego możliwościach, warto zajrzeć do oficjalnej dokumentacji https://fishshell.com/docs/current/index.html lub przetestować inne funkcje dostępne w tym języku, np. pobieranie informacji o czasie czy wykorzystanie zmiennych środowiskowych. Dodatkowo, jeśli chcesz poznać inne języki programowania, które również mają wbudowane funkcje do pobierania daty, zapraszamy do lektury naszych innych artykułów.

Dzięki prostym komendom w języku Fish Shell możemy szybko i wygodnie uzyskać bieżącą datę w formacie, który najlepiej odpowiada naszym potrzebom. Mamy nadzieję, że ten wpis okazał się dla Ciebie przydatny i pomoże w dalszej pracy z Fish Shell. Do zobaczenia!