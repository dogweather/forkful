---
title:    "Java: Пошук та заміна тексту"
keywords: ["Java"]
---

{{< edit_this_page >}}

**##Чому**

Пошук та заміна тексту - це важлива частина програмування в Java, оскільки дозволяє замінити велику кількість однакових елементів одним разом. Це зберігає час, зусилля та полегшує підтримку коду. 

**##Як це зробити**

```Java
public class Main {
  public static void main(String[] args) {
    String sentence = "Привіт всім! Сьогодні ми будемо вивчати Java.";
    String replacedSentence = sentence.replace("Java", "програмування");
    System.out.println(replacedSentence);
  }
}
```

**Вивід:**

Привіт всім! Сьогодні ми будемо вивчати програмування.

У цьому прикладі ми використовуємо метод `replace()` для пошуку та заміни рядка `"Java"` на `"програмування"`. Метод повертає новий рядок зі зміненим текстом. Можна також використовувати метод `replaceAll()` для заміни всіх співпадінь, а не лише першого.

```Java
public class Main {
  public static void main(String[] args) {
    String sentence = "У мене є 3 яблука та 4 апельсини.";
    String replacedSentence = sentence.replaceAll("[0-9]", "10");
    System.out.println(replacedSentence);
  }
}
```

**Вивід:**

У мене є 10 яблука та 10 апельсини.

У цьому прикладі ми використовуємо регулярний вираз `[0-9]` для пошуку всіх чисел у рядку і замінюємо їх на число `10`.

**##Глибока знахідка**

Пошук та заміна тексту також може бути використана для більш складних сценаріїв, таких як шаблони рядків або розширені регулярні вирази. Це дозволяє більш гнучко працювати з текстом та замінювати його за бажанням.

**##Дивись також**

1. [Офіційна документація Java для методу `replace()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
2. [Офіційна документація Java для методу `replaceAll()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
3. [Огляд регулярних виразів в Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)