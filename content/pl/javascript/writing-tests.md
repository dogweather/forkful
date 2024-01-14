---
title:                "Javascript: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów jest nieodzowną częścią procesu programowania. Pozwala ono na przetestowanie naszego kodu, upewnienie się o jego poprawności i zapobieganie błędom. Dodatkowo, dzięki testom, możemy łatwiej wprowadzać zmiany w naszym programie, ponieważ możemy szybko zweryfikować czy nie wprowadzają one niechcianych efektów.

## Jak napisać testy w Javascript

Aby napisać testy w języku Javascript, potrzebujemy odpowiedniej biblioteki, takiej jak Mocha czy Jest. Następnie, musimy zdefiniować nasze testy w odpowiedniej strukturze. Przykładowy kod testu wyglądałby następująco:

```Javascript 
it('powinno zwrócić sumę dwóch liczb', () => {
  const result = sum(2, 3);
  const expected = 5;
  assert.equal(result, expected);
});
```

W powyższym przykładzie widzimy, że najpierw definiujemy, co nasz test powinien zwrócić (w naszym przypadku sumę dwóch liczb). Następnie, deklarujemy oczekiwany wynik i porównujemy go z rzeczywistym wynikiem. Dzięki temu, jeśli test nie zwróci oczekiwanego wyniku, zostanie on uznany za nieudany.

## Głębsze zagadnienia

Tworzenie testów w języku Javascript może być trudniejsze ze względu na jego asynchroniczność. W takich przypadkach, musimy użyć struktury "done", aby oznaczyć, że nasz test jest kompletny. Przykład ten jest szczególnie ważny w przypadku testowania funkcji asynchronicznych, takich jak pobieranie danych z bazy danych.

``Javascript
it('powinno zwrócić listę użytkowników', (done) => {
  getUsers((users) => {
    const expected = ['John', 'Jane', 'Tom'];
    assert.deepEqual(users, expected);
    done();
  });
});
```

Najważniejszą rzeczą przy tworzeniu testów jest pisanie ich w sposób czytelny i zrozumiały dla innych programistów. Pamiętajmy także, że testy powinny być tworzone równolegle z kodem, aby zapewnić jego jakość.

## Zobacz także

- Artykuł na temat testowania w języku Javascript: [Link](https://developer.mozilla.org/pl/docs/Learn/JavaScript/Client-side_web_APIs/Introduction)
- Przykłady testów w bibliotece Jest: [Link](https://jestjs.io/docs/en/getting-started)