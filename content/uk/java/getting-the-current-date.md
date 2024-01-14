---
title:    "Java: Отримання поточної дати"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому

Отримання поточної дати є важливою частиною багатьох програм, оскільки ця інформація може бути використана для різних цілей, таких як логування, планування подій і створення звітів.

## Як отримати поточну дату

Найпростіший спосіб отримати поточну дату в Java - використовувати клас `Date` і його метод `getDate()`. Нижче наведені приклади коду і виводу:

```Java
import java.util.Date;

public class CurrentDate {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate.getDate());
    }
}
```

Вивід: `25`

Також можна отримати повну поточну дату і час, використовуючи клас `LocalDateTime` з пакету `java.time`:

```Java
import java.time.LocalDateTime;

public class CurrentDateTime {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        System.out.println(now);
    }
}
```

Вивід: `2021-09-25T15:13:06.573777`

## Глибше занурення

Якщо ви хочете отримати більш точну часову мітку або використовувати дату для додаткової обробки, ви можете використовувати клас `Instant` з пакету `java.time`. Цей клас представляє момент в часі з точністю до наносекунд. Нижче наведений приклад, який виводить поточний час у мілісекундах:

```Java
import java.time.Instant;

public class CurrentTime {
    public static void main(String[] args) {
        Instant now = Instant.now();
        System.out.println(now.toEpochMilli());
    }
}
```

Вивід: `1632585213783`

## Дивись також

- [Документація Java по класу Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Документація Java по класу LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Документація Java по класу Instant](https://docs.oracle.com/javase/8/docs/api/java/time/Instant.html)