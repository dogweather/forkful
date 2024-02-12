---
title:                "Логування"
date:                  2024-01-26T01:09:33.703334-07:00
model:                 gpt-4-1106-preview
simple_title:         "Логування"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/logging.md"
---

{{< edit_this_page >}}

## Що та чому?

Логування, як основа, - це практика запису подій і даних з програмного застосунку в зовнішній вивід, наприклад, у файл або консоль. Програмісти ведуть логи, щоб простежувати код, усувати проблеми та контролювати поведінку додатка в реальних умовах, забезпечуючи ключові інсайти, які не можна отримати так ефективно іншим способом.

## Як це зробити:

У Kotlin логування можна здійснити з використанням вбудованої функції `println()` для простих випадків або з використанням більш складних бібліотек, таких як SLF4J з Logback або Log4j для складніших потреб.

Нижче наведено базовий приклад з використанням `println()`:

```Kotlin
fun main() {
    println("Просте повідомлення логу: Застосунок запущено.")
    // ... тут деяка логіка застосунку ...
    try {
        // Симуляція помилки
        throw Exception("Симульована помилка")
    } catch (e: Exception) {
        println("Повідомлення логу помилки: " + e.message)
    }
}
```

Вивід:
```
Просте повідомлення логу: Застосунок запущено.
Повідомлення логу помилки: Симульована помилка
```

А ось фрагмент, що використовує SLF4J з налаштованим Logback:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Структуроване повідомлення логу: Додаток запущено.")
    // ... тут деяка логіка застосунку ...
    try {
        // Симуляція помилки
        throw Exception("Симульована помилка")
    } catch (e: Exception) {
        logger.error("Структурований лог помилки: ", e)
    }
}
```

Припускаючи відповідну конфігурацію Logback, вивід буде відформатований і може виглядати приблизно так, коли він записується у файл логів:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Структуроване повідомлення логу: Додаток запущено.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Структурований лог помилки: 
java.lang.Exception: Симульована помилка
   at com.myapp.Main.main(Main.kt:10)
```

## Поглиблений розгляд

Історично, логування у програмному забезпеченні розвивалося разом зі зростанням складності застосунків та систем. Прості оператори виводу були достатніми на ранніх стадіях, коли програми часто запускали та налагоджували самі розробники. Але як системи стали мережевими та працювали в різних середовищах для різних користувачів, надійна та стійка система логування стала ключовою.

Перед тим як Kotlin став популярним, розробники Java широко використовували бібліотеки як Log4j, а згодом SLF4J. Вони надихнули на подібні практики в Kotlin, використовуючи взаємодію Kotlin з бібліотеками Java. SLF4J виступає як шар абстракції, дозволяючи замінити реальну реалізацію логування - зазвичай Logback або Log4j2 є бажаними виборами.

Kotlin також дозволяє використовувати багатоплатформні рішення для логування, які працюють на JVM, JavaScript та Native, наприклад, через механізм `expect`/`actual`, який абстрагує реалізації специфічні для кожної платформи.

На відміну від спеціалізованих бібліотек для логування, println залишається найпростішою формою логування, оскільки не вимагає додаткових налаштувань або залежностей; однак, зазвичай воно не підходить для виробничих додатків через його недостатність можливостей, таких як рівні логів, ротація логів та структуровані формати.

Інші загальні можливості передових фреймворків логування включають:

- Рівні логів (DEBUG, INFO, WARN, ERROR тощо) для категоризації терміновості повідомлень логу.
- Вивід у різні джерела, як консоль, файл, бази даних або мережеві служби.
- Автоматична ротація логів та політики збереження.
- Підтримка розподіленого трасування для архітектури мікросервісів.
- Структуроване логування в форматах як JSON, які добре інтегровані з системами аналітики логів.

Ці інструменти та можливості критично важливі для підтримання надійної, спостережуваної системи особливо в складних, розподілених або високомасштабованих середовищах.

## Дивіться також

Для подальшого вивчення та отримання додаткових знань про логування в Kotlin, ознайомтеся з:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, наступник Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Документація Kotlin Multiplatform про декларації 'expect' та 'actual': [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Посібник зі структурованого логування в Kotlin: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)