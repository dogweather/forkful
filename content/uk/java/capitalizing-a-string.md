---
title:                "Перетворення рядка у великі літери"
html_title:           "Java: Перетворення рядка у великі літери"
simple_title:         "Перетворення рядка у великі літери"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Капіталізація рядків - це процес перетворення першої букви кожного слова в рядку на велику літеру. Це може бути корисним для полегшення читання і розпізнавання слів у великих текстових документах, а також для створення естетичної форматування.

## Як

```Java
public class CapitalizeString {

    public static String capitalize(String str) {
        StringBuilder result = new StringBuilder(str.length());

        // розділити строку на слова за допомогою методу split()
        String[] words = str.split("\\s");

        // перетворити першу букву кожного слова на велику і додати до результату
        for (String word : words) {
            result.append(Character.toUpperCase(word.charAt(0)))
                    .append(word.substring(1)).append(" ");
        }

        return result.toString();
    }

    public static void main(String[] args) {
        String sentence = "це приклад рядка для капіталізації";
        String capitalizedSentence = capitalize(sentence);
        System.out.println(capitalizedSentence);

        // результат: Це Приклад Рядка Для Капіталізації
    }
}
```

## Глибоке погруження

У цьому прикладі ми використовуємо метод `split()` для розділення рядка на окремі слова та метод `charAt()` для доступу до першої букви кожного слова. Ми також використовуємо метод `substring()` для отримання решти букв слова після першої. Завдяки цьому нам вдається перетворити тільки першу букву кожного слова без зміни решти тексту. Також варто зазначити, що цей приклад може бути покращеним для врахування різних варіацій роздільників між словами, а також для врахування великих літер у середині слова.

## Дивись також

- [Документація Java по методу `split()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#split-java.lang.String-)
- [Документація Java по методу `charAt()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#charAt-int-)
- [Документація Java по методу `substring()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)