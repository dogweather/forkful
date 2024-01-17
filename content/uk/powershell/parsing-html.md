---
title:                "Аналізування html"
html_title:           "PowerShell: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-html.md"
---

{{< edit_this_page >}}

# Що і чому?

Розбирання HTML - це процес отримання інформації з кодування веб-сторінки для подальшої обробки і аналізу. Веб-програмісти часто роблять це для отримання потрібної інформації з веб-сайту, такої як заголовки, текст або зображення, або для автоматизації рутинних задач.

# Як?

Для розбирання HTML з використанням PowerShell, ми можемо використовувати модуль `Invoke-WebRequest` (або `iwr` для короткості), який дозволяє нам звертатися до веб-сторінок і повертати їх вміст у зручному для обробки форматі.

```PowerShell
# Використовуємо команду `Invoke-WebRequest`, щоб отримати HTML сторінки:
$page = Invoke-WebRequest "https://uk.wikipedia.org/wiki/PowerShell" 
```

А потім ми можемо використовувати функцію `ParseHtml` з модулю `HTMLAgilityPack` для розбору отриманого HTML коду і отримання потрібних елементів, наприклад, усіх посилань на сторінці:

```PowerShell
# Використовуємо модуль `HTMLAgilityPack` для розбирання HTML:
$links = [HTMLAgilityPack.HtmlWeb]::Load($page).ParseHtml("//a")
```

І в кінці ми можемо вивести отримані дані для перевірки:

```PowerShell
# Выводим результат:
$links | Select-Object -Property "InnerText", "OuterHtml"
```

Результат:

```
InnerText                               OuterHtml
---------                               ---------
Ця стаття або ділянка містить різнома... <a href="/wiki/%D0%92%D0%B8%D0%BA%D0%B...
PowerShell                              <a href="/wiki/PowerShell" title="Po...
.+Falloff%29"                           <a href="/w/index.php?title=PowerSh... 
```

# Глибока пір

Розбір HTML не є новою концепцією, але разом з розвитком інтернету і веб-технологій, він залишається актуальним для програмістів, які хочуть отримати зручний доступ до даних з веб-сторінок. На відміну від ручного розбирання, розбір HTML за допомогою PowerShell може бути повністю автоматизованим, що дозволяє програмістам економити час і зусилля при роботі з великими обсягами даних.

Є також альтернативні способи розбирання HTML, наприклад, використання Python і бібліотеки `BeautifulSoup`, або використання веб-браузера та інструментів розробника для отримання HTML коду сторінки в режимі приватності.

Щоб детальніше ознайомитися з розбиранням HTML з використанням PowerShell, вам можуть бути корисні наступні посилання:

- [Microsoft документація по `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Стаття про розбирання HTML з використанням `HTMLAgilityPack`](https://devblogs.microsoft.com/scripting/use-the-htmlagilitypack-to-parse-html-fragment/)
- [GitHub репозиторій `HTMLAgilityPack`](https://github.com/zzsergant/HTML-Agility-Pack)
- [Книга "PowerShell for Sysadmins"](https://leanpub.com/powershell-for-sysadmins) - включає практичні приклади використання `Invoke-WebRequest` для розбирання HTML

# Схоже

Є багато різноманітних інструментів та підходів для розбирання HTML, тому що це важлива задача для веб-програмістів. Не бійтеся експериментувати та шукати оптимальні рішення для вашого проекту. Лише розібравшись з інструментами, ви зможете здійснювати розбирання HTML на високому рівні.

Я сподіваюся, ця стаття допомогла вам краще зрозуміти процес розбору HTML з використанням PowerShell і надихнула на нові ідеї для вашого програмування. Приємного кодування!