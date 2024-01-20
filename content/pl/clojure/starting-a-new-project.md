---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaczynając nowy projekt programistyczny, tworzysz podstawy dla swojej aplikacji. Robimy to, aby skonfigurować narzędzia, biblioteki i strukturę naszego kodu, co ułatwia późniejsze tworzenie.

## Jak to zrobić:

Tworzenie nowego projektu w Clojure jest proste dzięki narzędziu Leiningen. Oto przykładowy kod oraz output:

```Clojure
;; Aby zainstalować Leiningen
;; korzystając z konsoli Unix, wpisz:
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod +x lein
mv lein /usr/local/bin

;; Następnie, aby założyć nowy projekt, wpisz:
lein new moj-projekt 

;; Potwierdzenie nowego projektu
;; po wykonaniu powyższych komend daje takie dane:
Project created at /home/uzytkownik/moj-projekt
```

## Głębsze spojrzenie

Histotycznie, Clojure zostało stworzone w 2007 roku przez Richa Hickey'a jako język na platformę Java. Jeżeli chodzi o przygotowywanie nowych projektów, oprócz `Leiningen`, istnieje również narzędzie `Boot`, które jest alternatywą. Niemniej jednak, `Leiningen` jest zdecydowanie najbardziej popularne i posiada wiele pluginów, które ułatwiają proces tworzenia i zarządzania projektem. Szczegółowości implementacji skoncentrowane są na tworzeniu plików konfiguracyjnych, integracji z narzędziami zewnętrznymi (np. systemami CI/CD) oraz ustawieniu struktury katalogów.

## Zobacz też

1. [Oficjalna strona Clojure](https://clojure.org/)
2. [Dokumentacja Leiningen](https://leiningen.org/)
3. [Strona projektu Boot](https://github.com/boot-clj/boot)
4. [Przegląd narzędzi Clojure](https://clojure.org/guides/getting_started)
5. [Poradnik jak zacząć z Clojure](https://www.braveclojure.com/getting-started/)