---
title:    "Elm: Використання регулярних виразів"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Чому

Регулярні вирази є потужним інструментом для роботи зі строками у більшій кількості мов програмування, включаючи Elm. З їх допомогою можна швидко та ефективно виконувати операції зі строками, такі як пошук, заміна та перевірка на відповідність певному формату. 

## Як

```Elm 
import Regex exposing (..)

-- Пошук email адреси
Regex.find "([\\w-]+@([\\w-]+\\.)+[\\w-]+)" "Моя email адреса: test@example.com" 
-- Вивід: Just (group 0) "test@example.com"

-- Заміна номера телефону на шаблонний формат
Regex.replace (Regex.regex "\\d{3}-\\d{3}-\\d{4}") "\\d{3}-\\d{3}-\\d{4}" "Номер телефону: 123-456-7890"
-- Вивід: "Номер телефону: xxx-xxx-xxxx"
```

## Deep Dive

Окрім базових виразів і використання шаблонів, регулярні вирази в Elm також дозволяють виконувати розширені операції, такі як використання груп, складних пошуків та замін. Також вони підтримують усі стандартні ескейп-послідовності для спеціальних символів.

Для більш детального ознайомлення з регулярними виразами в Elm, рекомендуємо прочитати офіційну документацію на сайті [elm-lang.org](https://elm-lang.org/docs/regular-expressions) та спробувати написати свої власні приклади.

## Дивись Також

- [Офіційна документація з регулярних виразів в Elm](https://elm-lang.org/docs/regular-expressions)
- [Вікіпедія: Регулярні вирази](https://uk.wikipedia.org/wiki/%D0%A0%D0%B5%D0%B3%D1%83%D0%BB%D1%8F%D1%80%D0%BD%D0%B8%D0%B9_%D0%B2%D0%B8%D1%80%D0%B0%D0%B7) 
- [10 цікавих регулярних виразів для початківців](https://www.thepolyglotdeveloper.com/2015/05/useful-regular-expressions-for-beginners/)