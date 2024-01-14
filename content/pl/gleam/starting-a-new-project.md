---
title:                "Gleam: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiasz się, dlaczego powinieneś zacząć nowy projekt w Gleam? Oto kilka powodów:

- Gleam jest językiem programowania funkcyjnego, co oznacza, że pisanie kodu w nim jest łatwiejsze i bardziej wydajne.

- Gleam jest językiem statycznie typowanym, co oznacza, że błędy są wykrywane przed uruchomieniem programu, co oszczędza czas i frustrację.

- Gleam ma przyjazną dla programistów składnię, dzięki czemu kod jest czytelny i łatwy do zrozumienia dla innych osób.

## Jak to zrobić

Aby rozpocząć nowy projekt w Gleam, należy wykonać kilka prostych kroków:

1. Zainstaluj Gleam na swoim komputerze, korzystając z instrukcji na oficjalnej stronie [gleam.run](https://gleam.run/).

2. Utwórz nowy projekt, wpisując w terminalu `gleam new <nazwa projektu>`.

3. Rozpocznij pisanie kodu w pliku `src/main.gleam` używając składni Gleam.

4. Uruchom swój kod, wpisując w terminalu `gleam run`.

5. Ciesz się pisaniem programów w Gleam!

Poniżej znajduje się przykładowy kod w Gleam oraz jego wynik:

```Gleam
pub fn hello(name: String) {
  let message = "Witaj " ++ name
  io.println(message)
}

hello("Czytelniku")
```

Wynik:

```
Witaj Czytelniku
```

## Głębszy zanurzenie

Aby jeszcze lepiej zacząć swoją przygodę z Gleam, warto przeczytać dokumentację na oficjalnej stronie oraz zapoznać się z dostępnymi bibliotekami. Warto także dołączyć do społeczności Gleam na forach dyskusyjnych i mediach społecznościowych, gdzie można uzyskać pomoc od innych programistów i dzielić się swoimi doświadczeniami.

## Zobacz również

- [Oficjalna strona Gleam](https://gleam.run/)
- [Dokumentacja Gleam](https://gleam.run/documentation.html)
- [Repozytorium GitHub z przykładowymi projektami w Gleam](https://github.com/gleam-lang/gleam_sandbox)
- [Społeczność Gleam na Discordzie](https://discord.gg/RFpJCYf)
- [Społeczność Gleam na Reddit](https://www.reddit.com/r/gleamlang/)