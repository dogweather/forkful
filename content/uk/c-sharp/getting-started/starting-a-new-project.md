---
title:                "Починаємо новий проект"
aliases:
- uk/c-sharp/starting-a-new-project.md
date:                  2024-01-20T18:03:09.768982-07:00
model:                 gpt-4-1106-preview
simple_title:         "Починаємо новий проект"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що та Чому?
Починати новий проект — це як вирушати в невідому подорож із чистою карткою. Програмісти розпочинають нові проекти для вирішення проблем, тестування ідей або вивчення нових технологій.

## Як це зробити:
Створення нового проекту в C# можна розпочати від руки або через IDE, наприклад Visual Studio. Давайте зробимо це через командний рядок.

```C#
// Спершу встановіть .NET SDK, якщо ще не встановлено

// Створення нового консольного проекту C#
dotnet new console -o MyNewProject

// Перейдіть у каталог проекту
cd MyNewProject

// Запустіть проект, щоб переконатися, що все працює
dotnet run
```

Ось так виглядатиме результат:

```plaintext
Hello, World!
```

## Поглиблено:
Створення проектів у C# бере свій початок зі стандартної бібліотеки .NET. Раніше були інші підходи, типу Visual Studio рішення (solutions) та проекти, а також MonoDevelop. Сьогодні .NET SDK дає однакові можливості через командний рядок. Так, ви маєте більший контроль і зрозумілість процесу.

Основні типи проектів включають консольні додатки, бібліотеки класів, ASP.NET веб-додатки, і багато інших. Створення проекту визначає його структуру, залежності та цільову платформу (.NET Core, .NET 5/6, .NET Framework).

Команда `dotnet new` насправді є шаблоном. Ви можете створити власні шаблони або використовувати існуючі для пришвидшення розробки.

## Також дивіться:
- [Офіційна документація Microsoft по .NET CLI](https://docs.microsoft.com/dotnet/core/tools/)
- [Sample .NET CLI templates on GitHub](https://github.com/dotnet/templating/wiki/Available-templates-for-dotnet-new)
- [Туторіали ASP.NET Core для початківців](https://docs.microsoft.com/aspnet/core/getting-started/)
