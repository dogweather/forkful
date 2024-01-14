---
title:                "C#: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Чому

Цей пост призначений для вас, якщо ви хочете дізнатися, як завантажити веб-сторінку за допомогою програмування на мові C#. Це може бути корисним для веб-розробників, автоматизаторів, дослідників даних та інших фахівців, які потребують отримати вміст із веб-сайту для подальшої обробки.

# Як

Завантаження веб-сторінки за допомогою C# - це досить простий процес. Спочатку, вам потрібно встановити бібліотеку HtmlAgilityPack, яка допоможе нам парсити HTML-код сторінки. Після цього ми можемо використовувати клас WebClient для завантаження веб-сторінки за її URL-адресою. Ось приклад коду:

```C#
// Підключаємо бібліотеки
using System.Net; 
using HtmlAgilityPack;

// Використовуємо клас WebClient для завантаження сторінки
WebClient client = new WebClient();
string url = "https://www.example.com";
string htmlCode = client.DownloadString(url);

// Використовуємо клас HtmlDocument для парсінгу коду
HtmlDocument document = new HtmlDocument();
document.LoadHtml(htmlCode);

// Знаходимо необхідні нам елементи за їх тегами та ідентифікаторами
// Приклад - знаходження заголовків сторінки
HtmlNodeCollection headers = document.DocumentNode.SelectNodes("//h1[@class='title']");

// Виводимо результат
Console.WriteLine("Заголовки сторінки:");
foreach (HtmlNode header in headers)
{
    Console.WriteLine(header.InnerText);
}

// Виводимо загальну кількість посилань на сторінці
HtmlNodeCollection links = document.DocumentNode.SelectNodes("//a[@href]");
Console.WriteLine("Кількість посилань: " + links.Count);
```

Вихід:

```
Заголовки сторінки:
Це заголовок сторінки
Кількість посилань: 10
```

Звичайно, ви можете використовувати інші теги та ідентифікатори для пошуку необхідної вам інформації на сторінці.

# Deep Dive

Якщо ви хочете дізнатися більше про завантаження веб-сторінки за допомогою C#, вам варто розглянути такі теми:

- Робота з Url-адресами, передача параметрів у запиті
- Парсінг нестандартних сторінок, які використовують JavaScript або Ajax
- Збереження вмісту сторінки у файл або базу даних

Навчальний матеріал з цих тем можна знайти у документації HtmlAgilityPack та на блогах та форумах з програмування.

# Дивіться також

- [Документація HtmlAgilityPack](https://html-agility-pack.net/)
- [Офіційний сайт C#](https://docs.microsoft.com/uk-ua/dot