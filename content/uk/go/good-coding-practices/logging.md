---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:45.078742-07:00
description: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0432 \u0440\
  \u043E\u0437\u0440\u043E\u0431\u0446\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043D\u043E\u0433\u043E \u0437\u0430\u0431\u0435\u0437\u043F\u0435\u0447\u0435\u043D\
  \u043D\u044F - \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0437\u0430\u043F\
  \u0438\u0441\u0443 \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457\
  \ \u043F\u0440\u043E \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0438, \u043F\u0440\u0438\u0437\u043D\u0430\
  \u0447\u0435\u043D\u0438\u0439 \u0434\u043B\u044F \u0432\u0456\u0434\u0441\u0442\
  \u0435\u0436\u0435\u043D\u043D\u044F \u0457\u0457 \u043F\u043E\u0432\u0435\u0434\
  \u0456\u043D\u043A\u0438 \u0442\u0430\u2026"
lastmod: '2024-03-13T22:44:48.446487-06:00'
model: gpt-4-0125-preview
summary: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0432 \u0440\u043E\
  \u0437\u0440\u043E\u0431\u0446\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043D\
  \u043E\u0433\u043E \u0437\u0430\u0431\u0435\u0437\u043F\u0435\u0447\u0435\u043D\u043D\
  \u044F - \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0437\u0430\u043F\u0438\
  \u0441\u0443 \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u043F\
  \u0440\u043E \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0438, \u043F\u0440\u0438\u0437\u043D\u0430\u0447\
  \u0435\u043D\u0438\u0439 \u0434\u043B\u044F \u0432\u0456\u0434\u0441\u0442\u0435\
  \u0436\u0435\u043D\u043D\u044F \u0457\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\
  \u043D\u043A\u0438 \u0442\u0430\u2026"
title: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F"
---

{{< edit_this_page >}}

## Що та чому?

Логування в розробці програмного забезпечення - це процес запису інформації про виконання програми, призначений для відстеження її поведінки та діагностики проблем. Програмісти впроваджують логування для моніторингу продуктивності програмного забезпечення, налагодження помилок, а також забезпечення безпеки системи та відповідності вимогам, роблячи його незамінним інструментом для обслуговування та аналізу додатків.

## Як це зробити:

В Go логування може бути реалізоване за допомогою стандартного пакету бібліотеки `log`. Цей пакет надає прості можливості логування, такі як запис у стандартний вивід або в файли. Почнемо з основного прикладу логування до стандартного виводу:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Це базовий запис у лог.")
}
```

Вивід:
```
2009/11/10 23:00:00 Це базовий запис у лог.
```

Мітка часу на початку запису в лог автоматично додається пакетом `log`. Далі давайте розглянемо, як логувати у файл, замість стандартного виводу:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Цей запис у лог йде у файл.")
}
```

Тепер розглянемо більш передовий випадок використання: налаштування формату логування. Go дозволяє створити власний логер за допомогою 'log.New()':

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "СПЕЦІАЛЬНИЙ ЛОГ: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Це власне повідомлення логу.")
}
```

Вивід:
```
СПЕЦІАЛЬНИЙ ЛОГ: 2009/11/10 23:00:00 main.go:11: Це власне повідомлення логу.
```

Цей приклад додає префікс "СПЕЦІАЛЬНИЙ ЛОГ: " до кожного повідомлення в логу та включає дату, час і місцезнаходження вихідного файлу.

## Поглиблено

Стандартний пакет `log` бібліотеки Go є простим і достатнім для багатьох застосунків, але йому бракує деяких більш витончених особливостей, що знаходяться в сторонніх бібліотеках логування, таких як структуроване логування, ротація логів та логування залежно від рівня. Пакети на кшталт `zap` та `logrus` пропонують ці передові можливості та мають високу оцінку в спільноті Go за їхню продуктивність і гнучкість.

Наприклад, структуроване логування дозволяє записувати дані в структурованому форматі (як JSON), що особливо корисно для сучасних хмарних застосунків, де логи можуть аналізуватися різними інструментами або сервісами. `zap` зокрема відомий своєю високою продуктивністю та низьким обсягом алокацій, що робить його придатним для застосунків, де критично важливі швидкість та ефективність.

Історично, логування в Go значно еволюціонувало з моменту створення мови. Ранні версії Go надавали базові можливості логування, які ми бачимо в пакеті `log`. Однак, з ростом популярності мови та складності застосунків, написаних на Go, спільнота почала розробляти більш передові бібліотеки логування, аби задовольнити свої потреби. Сьогодні, хоча стандартний пакет `log` залишається прийнятним варіантом для простих застосунків, багато розробників звертаються до цих сторонніх рішень для більш складних вимог до логування.
