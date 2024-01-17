---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Clojure: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Przetwarzanie daty na ciąg znaków to proces, w którym zamieniamy datę zapisaną w formacie liczbowym na czytelną dla ludzi postać. Programiści często wykonują to w celu wyświetlenia daty użytkownikom lub zapisu jej w pliku lub bazie danych. 

## Jak to zrobić:

Przykłady kodu i wyników znajdziesz poniżej:
```Clojure
;; Przykład 1:
(time (format "%1$td-%1$tm-%1$tY" (local-time)))
"24-08-2021"

;; Przykład 2:
(time (format "%1$tA, %1$tB %1$td, %1$tY" (timezone-adjust (date)))
"wtorek, sierpnia 24, 2021"
```

## Głębsza analiza:

URL: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html 
URL: https://clojuredocs.org/clojure.instant/set-zone

Aby przekonwertować datę do ciągu znaków, możemy skorzystać z funkcji ```format```. Ta funkcja korzysta z formatowania danych, podobnego do składni znanej z języka Java, która jest wykorzystywana w większości języków programowania. W Clojure można także użyć funkcji ```local-time``` lub ```date```, aby uzyskać aktualną datę w postaci liczbowej. Konwersja daty do ciągu znaków może być również wykonywana w inny sposób, na przykład za pomocą funkcji z biblioteki java-time, jednak formatowanie danych wymaga znajomości odpowiedniej składni.

## Zobacz także:

URL: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html 
URL: https://clojuredocs.org/clojure.time/format 
URL: https://clojuredocs.org/clojure.instant/ZonedDateTime