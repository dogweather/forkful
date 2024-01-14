---
title:    "C#: Приведення рядка до великих літер"
keywords: ["C#"]
---

{{< edit_this_page >}}

У програмуванні існує багато різних завдань, які потребують вміння працювати зі стрічками. Однією з них є капіталізація стрічки - процес перетворення всіх букв у верхній регістр. Це може бути корисно, наприклад, при роботі з іменами чи заголовками. У цій статті ми розглянемо, як це можна зробити у мові програмування C#.

## Як це зробити?

Для початку нам потрібно зберегти стрічку, з якою ми будемо працювати, у змінну типу `string`. Для цього використовуємо ключове слово `string` та давайте назовемо нашу змінну `myString`.

```C#
string myString = "привіт світ";
```

Тепер ми можемо використовувати метод `ToUpper()` для кожної букви у нашій стрічці і отримати капіталізований варіант:

```C#
string capitalizedString = myString.ToUpper();
Console.WriteLine(capitalizedString);
```

Результат виконання такого коду буде: `ПРИВІТ СВІТ`.

## Глибші відомості

Існує також можливість використовувати метод `ToTitleCase()`, який буде капіталізувати лише першу букву кожного слова у стрічці. Це може бути корисно при роботі з назвами чи фразами.

```C#
string titleCaseString = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(myString);
Console.WriteLine(titleCaseString);
```

Результат: `Привіт Світ`.

Також варто зазначити, що обидва методи не змінюють оригінальну стрічку, а повертають нову змінну з результатом.

## Дивись також

- [Документація C# для методу ToUpper()](https://docs.microsoft.com/uk-ua/dotnet/api/system.string.toupper?view=net-5.0)
- [Документація C# для методу ToTitleCase()](https://docs.microsoft.com/uk-ua/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- [Корисні функції для роботи зі стрічками у C#](https://www.pluralsight.com/guides/joining-and-splitting-strings-using-regular-expressions-in-c-sharp)