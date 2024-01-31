---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Коли кажуть "зробити першу літеру рядка великою" (capitalize a string), мають на увазі заміну першої маленької літери на велику. Програмісти це роблять, щоби текст виглядав охайніше, наприклад, для імен та назв у документації або користувацьких інтерфейсах.

## Як зробити:
```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

// Приклад використання
console.log(capitalizeFirstLetter('привіт')); // Вивід: "Привіт"
console.log(capitalizeFirstLetter('мир')); // Вивід: "Мир"
```

## Поглиблений огляд:
Функція зроблення першої літери великою в JavaScript не існує на вродженому рівні, відтак кожен розробник реалізує її самостійно. Історично виникла потреба кращої читабельності текстів, де важливі слова починаються з великої літери. В інших мовах, як CSS або SQL, існують вбудовані функції для форматування рядка. У JavaScript, простий шаблон полягає у використанні методів `charAt`, `toUpperCase`, та `slice` для досягнення бажаного результату. 

## Додаткові ресурси:
- MDN Web Docs про методи рядків: [MDN String methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Керівництво по стилю ECMAScript: [Airbnb JavaScript Style Guide](https://github.com/airbnb/javascript)
- W3Schools JavaScript String Reference: [W3Schools String Reference](https://www.w3schools.com/jsref/jsref_obj_string.asp)
