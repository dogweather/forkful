---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Arduino: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków odpowiadających wzorcowi może być niezbędne, gdy próbujemy filtrować lub przetwarzać dane, na przykład usuwając błędne znaki z odczytu z czujnika.

## Jak to zrobić

Aby usunąć znaki odpowiadające wzorcowi w programie Arduino, możemy wykorzystać funkcję `strstr()`. Przykładowy kod wygląda następująco:

```
//deklaracja zmiennych
char zdanie[] = "To jest przykładowe zdanie";
char wzorzec[] = "jest";
char *wystapienie;

void setup() {
  //ustawienie komunikacji z Monitorem szeregowym
  Serial.begin(9600);
  
  //znajdowanie pierwszego wystąpienia wzorca w zdaniu
  wystapienie = strstr(zdanie, wzorzec); 
  
  //jeśli znaleziono, to usuwamy znaki odpowiadające wzorcowi i wyświetlamy zmienione zdanie
  if(wystapienie != NULL){ 
    do{
      memmove(wystapienie, wystapienie + strlen(wzorzec), 1);
    }while (wystapienie != NULL);
    Serial.println(zdanie);
  }
}

void loop() {
  //nie potrzebujemy operować na danych w pętli, więc zostawiamy ją pustą
}
```

W powyższym przykładzie najpierw deklarujemy zmienne `zdanie` i `wzorzec`, a następnie ustawiamy komunikację z Monitorem szeregowym. Następnie za pomocą funkcji `strstr()` znajdujemy pierwsze wystąpienie wzorca w zdaniu. Jeśli jest ono znalezione, to wykorzystujemy pętlę do usuwania kolejnych wystąpień znaków odpowiadających wzorcowi za pomocą funkcji `memmove()`, a następnie wyświetlamy zmienione zdanie. 

## Głębsza analiza

Funkcja `strstr()` zwraca wskaźnik do miejsca w ciągu, w którym występuje pierwsze wystąpienie wzorca. W przypadku powyższego kodu, jest to początek słowa "jest". Aby usunąć znaki odpowiadające wzorcowi, przeznaczamy wystąpienie do funkcji `memmove()`, która przesuwa kolejne znaki o długość wzorca. Dzięki temu kolejne wystąpienia wzorca są nadpisane przez kolejne znaki w ciągu, aż nie będzie już żadnego wystąpienia. Należy jednak pamiętać, że w przypadku gdy długość wzorca nie jest równa długości znaku, może dojść do problemów z pamięcią.

## Zobacz też

- [Funkcja strstr() w dokumentacji Arduino](https://www.arduino.cc/reference/en/language/functions/strings/strstr/)
- [Przykłady z dokumentacji Arduino](https://www.arduino.cc/en/Tutorial/StringIndexOf)