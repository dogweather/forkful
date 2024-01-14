---
title:                "Fish Shell: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest nieodzownym elementem wielu działań programistycznych. Czasami potrzebujemy szybko uzyskać dostęp do danej strony, przykładowo w celu pozyskania informacji lub przetworzenia jej zawartości w innym programie. W tym wpisie dowiesz się, jak użyć Fish Shell do pobierania stron internetowych.

## Jak To Zrobić

Fish Shell nie posiada wbudowanej funkcjonalności do pobierania stron internetowych, jednak za pomocą kilku poleceń możemy szybko i łatwo uzyskać dostęp do zawartości strony. Aby pobrać stronę, użyjemy polecenia `curl` wraz z adresem URL strony. Przykładowo, jeśli chcemy pobrać stronę google.com, wpiszemy:

```Fish Shell
$ curl http://www.google.com
```
 
W rezultacie powinniśmy uzyskać kod HTML strony w konsoli, co oznacza, że pomyślnie pobraliśmy stronę. Jeśli chcemy zapisać zawartość strony do pliku, możemy użyć opcji `-o` oraz podać nazwę pliku, do którego chcemy zapisać dane. W tym przypadku polecenie będzie wyglądać następująco:

```Fish Shell
$ curl -o google.html http://www.google.com
```

Zawartość strony zostanie zapisana do pliku `google.html`, który możemy potem otworzyć w przeglądarce internetowej lub przetworzyć w innym programie.

## Deep Dive

Pobieranie stron internetowych przy użyciu Fish Shell może odbywać się na wiele sposobów i z wykorzystaniem różnych poleceń. Jednym z popularniejszych sposobów jest użycie poleceń `curl` lub `wget`. Oba umożliwiają pobranie zawartości strony, jednak różnią się nieco w sposobie działania. 

Polecenie `curl` zostało już użyte w przykładach powyżej i jest bardzo wszechstronne, umożliwiając pobieranie nie tylko stron internetowych, ale też innych rodzajów danych. Natomiast `wget` jest bardziej wyspecjalizowany w pobieraniu strony internetowej i oferuje dodatkowe funkcjonalności, takie jak możliwość wznawiania pobierania w przypadku przerwanej transmisji.

Warto zaznaczyć, że niektóre strony internetowe mogą blokować pobieranie zawartości w ten sposób, dlatego warto przetestować różne podejścia i wybrać to, które najlepiej zadziała dla danej strony.

## Zobacz Również

Jeśli interesuje Cię więcej sposobów na pobieranie stron internetowych, warto zapoznać się z dokumentacją Fish Shell oraz opracowaniami na temat poleceń `curl` i `wget`, które użyliśmy w tym wpisie.

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Dokumentacja polecenia curl](https://curl.haxx.se/docs/manpage.html)
- [Dokumentacja polecenia wget](https://www.gnu.org/software/wget/manual/wget.html)