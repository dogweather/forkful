---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:51.458951-07:00
description: "\u042F\u043A: PowerShell \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\
  \u043E\u0441\u0442\u0456 \u0432\u0438\u043A\u043B\u0438\u043A\u0438 \u043A\u043E\
  \u043C\u0430\u043D\u0434 \u0434\u043B\u044F \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\u0441\u0443\
  . \u041A\u043E\u043C\u0430\u043D\u0434\u043B\u0435\u0442 `Get-Date` \u0454 \u043E\
  \u0441\u043D\u043E\u0432\u043D\u0438\u043C \u0456\u043D\u0441\u0442\u0440\u0443\u043C\
  \u0435\u043D\u0442\u043E\u043C \u0434\u043B\u044F \u0446\u0456\u0454\u0457 \u043C\
  \u0435\u0442\u0438. \u0412\u0456\u043D \u043C\u043E\u0436\u0435 \u043F\u043E\u0432\
  \u0435\u0440\u0442\u0430\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.668094-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\u043E\u0441\u0442\
  \u0456 \u0432\u0438\u043A\u043B\u0438\u043A\u0438 \u043A\u043E\u043C\u0430\u043D\
  \u0434 \u0434\u043B\u044F \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\u0441\u0443."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## Як:
PowerShell надає прості виклики команд для отримання дати та часу. Командлет `Get-Date` є основним інструментом для цієї мети. Він може повертати повну дату та час, які ви можете форматувати або маніпулювати відповідно до своїх потреб.

```powershell
# Отримати поточну дату та час
Get-Date
```

**Приклад виводу:**

```
Вівторок, 5 вересня 2023 р. 9:46:02
```

Ви також можете відформатувати вивід, щоб він відображав лише потрібну вам інформацію, наприклад, лише дату або лише час.

```powershell
# Отримати лише поточну дату у конкретному форматі
Get-Date -Format "yyyy-MM-dd"
```

**Приклад виводу:**

```
2023-09-05
```

```powershell
# Отримати лише поточний час
Get-Date -Format "HH:mm:ss"
```

**Приклад виводу:**

```
09:46:02
```

### Використання класу .NET
PowerShell дозволяє безпосередній доступ до класів .NET, пропонуючи альтернативний спосіб роботи з датами і часом.

```powershell
# Використання класу DateTime .NET для отримання поточної дати та часу
[System.DateTime]::Now
```

**Приклад виводу:**

```
Вівторок, 5 вересня 2023 р. 9:46:02
```

Для UTC часу:

```powershell
# Використання класу DateTime .NET для отримання поточної дати та часу UTC
[System.DateTime]::UtcNow
```

**Приклад виводу:**

```
Вівторок, 5 вересня 2023 р. 13:46:02
```

Ці команди та класи забезпечують потужні та гнучкі варіанти для роботи з датами й часом у PowerShell, що є невід'ємною частиною багатьох скриптів та автоматизаційних завдань.
