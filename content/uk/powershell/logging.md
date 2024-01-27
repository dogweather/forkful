---
title:                "Логування"
date:                  2024-01-26T01:07:40.799491-07:00
model:                 gpt-4-1106-preview
simple_title:         "Логування"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/logging.md"
---

{{< edit_this_page >}}

## Що і Чому?
Логгінг, по суті, це створення сліду з хлібних крихт крізь ваш код - це спосіб відстеження того, що відбувається, коли ваш сценарій працює в "дикій" природі. Програмісти ведуть лог для відлагодження, слідкування за поведінкою додатку, моніторингу продуктивності та спостереження за будь-якими неподобствами.

## Як це зробити:
Ось керівництво щодо додавання базового логгінга до ваших сценаріїв:

```PowerShell
# Створення простого повідомлення у лог
Write-Host "Інфо: Запуск процесу сценарію."

# Запис у файл
"Інфо: Це повідомлення записано у лог." | Out-File -Append myLog.log

# Використання вбудованої cmdlet для більш детального логгінга
Start-Transcript -Path "./detailedLog.log"
Write-Output "Попередження: Щось не зовсім вірно."
# ... ваш сценарій щось виконує
Stop-Transcript

# Вивід detailedLog.log
******************************
Запис початку роботи Windows PowerShell
Час початку: 20230324112347
Користувач: PShellGuru@example.com
Користувач з правами адміністратора: PShellGuru@example.com
Назва конфігурації: 
Машина: PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Програма-хост: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ID процесу: 2024
Версія PS: 7.1.2
```

Тепер у ваших логах є хронологія того, чим займався ваш код.

## Поглиблений Захід:
Історично логгінг сипека скільки саме програмування. Це як бортовий журнал капітана, але для програмного забезпечення. У минулому це могли бути роздруківки або телетайпи; зараз мова йде про файли та розкішні системи управління логами.

Коли ви занурюєтеся в траншеї PowerShell, `Write-Host` - це швидко та нечисто, але воно просто виводить текст у консоль, не надто підходить для зберігання записів. `Out-File` дає вам простий спосіб закинути текст у файл, але для справжнього соку вам захочеться використовувати `Start-Transcript` та `Stop-Transcript`, які реєструють все - вхідні, вихідні дані, усе на світі.
Альтернативи? Звичайно, якщо ви працюєте у великому підприємстві, ви могли б розглянути Реєстрацію Подій Windows або використання програмного забезпечення такого як Logstash, але для вашого щоденного сценарію залишайтеся з інструментами PowerShell. Що стосується реалізації, пам'ятайте, щоб логінг був розумним – занадто мало та він безглуздий, занадто багато та це білий шум.

## Дивіться Також:
Перегляньте це, щоб узнати все про логгінг в PowerShell: