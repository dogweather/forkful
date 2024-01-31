---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Регулярні вирази – це шаблони для пошуку та маніпуляції текстом. Програмісти використовують їх для валідації, парсингу, трансформації тексту – сильно спрощують і автоматизують роботу з даними.

## Як зробити:
```Clojure
;; Пошук по шаблону
(re-find #"\bClove?r\b" "Clojure and Clovr are both variations of Clove.") ; => "Clovr"

;; Заміна за шаблоном
(clojure.string/replace "7 котів, 9 собак" #"\d" "#") ; => "# котів, # собак"

;; Розділення строки по шаблону
(clojure.string/split "Київ, Львів; Одеса, Харків" #"[,;] ") ; => ["Київ" "Львів" "Одеса" "Харків"]

;; Перевірка відповідності шаблону
(boolean (re-matches #"\d{3}-\d{2}-\d{2}" "123-45-67")) ; => true
```

## Поглиблено:
Регулярні вирази з'явилися ще в 1950-х, коли Стівен Кліні створив теоретичні основи. Альтернативи include готові бібліотеки парсингу, але вони можуть бути перегруженими для простих задач. Регулярні вирази в Clojure реалізовані через Java Pattern API, що гарантує високу продуктивність.

## Див. також:
- [ClojureDocs по регулярним виразам](https://clojuredocs.org/clojure.core/re-find)
- [Java Pattern API](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Learn Regex the Hard Way](https://regex.learncodethehardway.org/)
