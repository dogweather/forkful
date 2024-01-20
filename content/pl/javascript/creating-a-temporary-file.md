---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowego pliku polega na generowaniu krótkotrwałego miejsca do przechowywania danych, które są użyteczne tylko przez ograniczony czas. Programiści robią to, gdy potrzebują miejsca na tymczasowe przechowywanie informacji, które są niezbędne dla bieżącego zadania, ale nie są potrzebne na stałe.

## Jak to zrobić:
W JavaScript możemy użyć modułu `tmp`, który dostarcza wygodne funkcje do tworzenia plików tymczasowych. Oto prosty przykład:

```Javascript
var tmp = require('tmp');

tmp.file(function errCreate(err, path, fd, cleanupCallback) {
  if (err) throw err;

  console.log("Scieżka tymczasowego pliku: ", path);
  console.log("Deskryptor pliku: ", fd);

  // Czyść po sobie
  cleanupCallback();
});
```

Po uruchomieniu powyższego kodu możemy zobaczyć ścieżkę do tymczasowego pliku i jego deskryptor pliku.

## W głębi tematu:
Historia JavaScript nie jest pełna informacji na temat tworzenia plików tymczasowych, ponieważ większość operacji przetwarzania plików jest zazwyczaj realizowana na poziomie serwera, na przykład z wykorzystaniem Node.js. Alternatywą dla tworzenia plików tymczasowych jest korzystanie z pamięci podręcznej lub sesji. Wybór metody zależy od specyfiki zadania. Twórzenie plików tymczasowych jest szczególnie użyteczne, gdy mamy do czynienia z dużymi ilościami danych, które przekraczają pojemność pamieci podręcznej lub sesji.

## Zobacz również:
Aby dowiedzieć się więcej, zajrzyj na poniższe strony:

1. Oficjalna dokumentacja `tmp` [dostępna tutaj](https://www.npmjs.com/package/tmp)