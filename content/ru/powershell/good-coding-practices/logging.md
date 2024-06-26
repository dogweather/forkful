---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:25.652021-07:00
description: "\u041A\u0430\u043A: \u0412\u043E\u0442 \u043A\u043E\u0440\u043E\u0442\
  \u043A\u043E\u0435 \u0440\u0443\u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E\
  \ \u043F\u043E \u0434\u043E\u0431\u0430\u0432\u043B\u0435\u043D\u0438\u044E \u0431\
  \u0430\u0437\u043E\u0432\u043E\u0433\u043E \u043B\u043E\u0433\u0438\u0440\u043E\u0432\
  \u0430\u043D\u0438\u044F \u0432 \u0432\u0430\u0448\u0438 \u0441\u043A\u0440\u0438\
  \u043F\u0442\u044B."
lastmod: '2024-03-13T22:44:45.463761-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u043E\u0440\u043E\u0442\u043A\u043E\u0435 \u0440\
  \u0443\u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u043F\u043E \u0434\
  \u043E\u0431\u0430\u0432\u043B\u0435\u043D\u0438\u044E \u0431\u0430\u0437\u043E\u0432\
  \u043E\u0433\u043E \u043B\u043E\u0433\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F\
  \ \u0432 \u0432\u0430\u0448\u0438 \u0441\u043A\u0440\u0438\u043F\u0442\u044B."
title: "\u0416\u0443\u0440\u043D\u0430\u043B\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u0435"
weight: 17
---

## Как:
Вот короткое руководство по добавлению базового логирования в ваши скрипты:

```PowerShell
# Создание простого сообщения в лог
Write-Host "Инфо: Начало процесса скрипта."

# Запись в файл
"Инфо: Это сообщение залогировано." | Out-File -Append myLog.log

# Использование встроенного cmdlet для более детализированного логирования
Start-Transcript -Path "./detailedLog.log"
Write-Output "Предупреждение: Что-то не так."
# ... ваш скрипт что-то делает
Stop-Transcript

# Вывод detailedLog.log
******************************
Запись транскрипции PowerShell начата
Время начала: 20230324112347
Пользователь: PShellGuru@example.com
Пользователь с правами запуска: PShellGuru@example.com
Имя конфигурации: 
Компьютер: PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Хост-приложение: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ID Процесса: 2024
Версия PS: 7.1.2
```

Теперь в ваших логах есть пошаговое описание того, что делал ваш код.

## Глубокое погружение:
Исторически, логирование почти так же старо, как и программирование. Это как журнал капитана, но для программного обеспечения. В старые времена это могли быть распечатки или телетайпные машины; сейчас это уже файлы и сложные системы управления логами.

Когда вы находитесь в "окопах" PowerShell, `Write-Host` быстро и грязно, но он просто выводит текст на консоль, что не очень хорошо для ведения записей. `Out-File` предоставляет простой способ вывести текст в файл, но для настоящего "сока" вы захотите использовать `Start-Transcript` и `Stop-Transcript`, которые логируют всё – ввод, вывод, весь процесс.

Альтернативы? Конечно, если вы работаете на предприятии, вы могли бы посмотреть на Журнал событий Windows или использовать программное обеспечение вроде Logstash, но для вашего ежедневного скрипта оставайтесь с инструментами PowerShell. Что касается реализации, помните, что нужно логировать умно – слишком мало и это бесполезно, слишком много и это белый шум.

## Смотрите также:
Проверьте это, чтобы взять в руки всё, что касается логирования в PowerShell:
