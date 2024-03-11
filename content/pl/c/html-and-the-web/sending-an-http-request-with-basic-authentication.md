---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:53.920973-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnieniem\
  \ w j\u0119zyku C polega na przygotowaniu \u017C\u0105dania HTTP, kt\xF3re zawiera\
  \ nag\u0142\xF3wek Authorization z\u2026"
lastmod: '2024-03-11T00:14:09.102177-06:00'
model: gpt-4-0125-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnieniem w\
  \ j\u0119zyku C polega na przygotowaniu \u017C\u0105dania HTTP, kt\xF3re zawiera\
  \ nag\u0142\xF3wek Authorization z\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnianiem"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem w języku C polega na przygotowaniu żądania HTTP, które zawiera nagłówek Authorization z zakodowanymi w Base64 poświadczeniami użytkownika. Jest to powszechna metoda dodawania prostej warstwy uwierzytelniania do żądań HTTP, pozwalająca na programowe dostępowanie do zasobów ograniczonych.

## Jak to zrobić:
Aby wysłać żądanie HTTP z podstawowym uwierzytelnieniem w języku C, będziemy potrzebować użyć biblioteki libcurl, popularnej, wszechstronnej i łatwej w użyciu biblioteki do transferu URL po stronie klienta. Obsługuje ona różne protokoły, w tym HTTP i HTTPS, co upraszcza nasze zadanie. Upewnij się, że libcurl jest zainstalowana w twoim systemie przed kontynuowaniem. Oto podstawowy przykład demonstrujący, jak wysłać żądanie GET z podstawowym uwierzytelnieniem:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // Adres URL, pod który wysyłane jest żądanie
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Włączenie użycia podstawowego uwierzytelniania
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Podanie nazwy użytkownika i hasła dla podstawowego uwierzytelniania
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Wykonanie żądania GET
        res = curl_easy_perform(curl);

        // Sprawdzanie błędów
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Zawsze sprzątaj
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
W powyższym przykładzie zamień `"http://example.com/resource"`, `"username"` i `"password"` na swój rzeczywisty adres URL, nazwę użytkownika i hasło.

Kod ten inicjalizuje obiekt `CURL`, ustawia adres URL, włącza uwierzytelnianie podstawowe HTTP i określa poświadczenia. Następnie wysyła żądanie i sprząta po sobie. Jeśli zakończy się sukcesem, żądany zasób zostaje pobrany; w przypadku błędu, jest on wyświetlany na stderr.

Przykładowe wyjście (przy założeniu, że uwierzytelnianie i dostęp do zasobu są udane) może nie być bezpośrednio pokazane przez program, ponieważ przykład głównie demonstruje wysyłanie żądania. Aby wydrukować odpowiedź, należałoby rozszerzyć program o obsługę danych odpowiedzi HTTP.

## Glebsze zanurzenie:
Wysyłanie żądań HTTP z podstawowym uwierzytelnieniem w języku C, jak pokazano, wykorzystuje bibliotekę libcurl ze względu na jej niezawodność i prostotę. Historycznie, tworzenie żądań HTTP czysto w C bez takich bibliotek było uciążliwe i narażone na błędy, wymagało programowania na niższym poziomie z gniazdami i ręcznym konstruowaniem nagłówków HTTP.

Samo podstawowe uwierzytelnianie to metoda z wczesnych dni internetu. Wysyła poświadczenia w łatwo dekodowalnym formacie (Base64), co jest niebezpieczne na niezabezpieczonych kanałach. Nowoczesne aplikacje często preferują bezpieczniejsze metody uwierzytelniania, takie jak OAuth 2.0 lub JWT (JSON Web Tokens), szczególnie dla wrażliwych danych.

Jednakże, dla wewnętrznych, mniej krytycznych systemów, lub szybkich i brudnych skryptów, gdzie wygoda przeważa nad obawami dotyczącymi bezpieczeństwa, podstawowe uwierzytelnienie nadal jest używane. Ponadto, połączone z szyfrowanymi połączeniami (HTTPS), jego prostota staje się zaletą dla szybkiego rozwoju, testowania lub prac automatyzacyjnych, gdzie wyższy poziom mechanizmów bezpieczeństwa nie jest tak konieczny.

W kontekstach, gdzie nowoczesne bezpieczeństwo jest niezbędne, należy priorytetowo traktować alternatywy, takie jak uwierzytelnianie oparte na tokenach. Niemniej jednak, zrozumienie, jak zaimplementować podstawowe uwierzytelnianie w C za pomocą libcurl, dostarcza podstawową umiejętność, która może być dostosowana do różnych metod uwierzytelniania i protokołów, odzwierciedlając subtelne kompromisy pomiędzy bezpieczeństwem, wygodą i wymaganiami aplikacji w rozwoju webowym.
