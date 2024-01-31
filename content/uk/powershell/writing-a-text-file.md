---
title:                "Створення текстового файлу"
date:                  2024-01-19
simple_title:         "Створення текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Створення текстового файлу - це запис даних у формі тексту на диск, доступний для читання і редагування. Програмісти роблять це, щоб зберігати інформацію, логи, налаштування програм, чи для обміну даними між процесами.

## Як це зробити:
### Створення та запис у файл:
```PowerShell
$text = "Привіт, Україно!"
Set-Content -Path "C:\path\to\file.txt" -Value $text
```

### Додавання тексту до існуючого файлу:
```PowerShell
Add-Content -Path "C:\path\to\file.txt" -Value "Додаємо текст"
```

### Читання з файлу (для перевірки):
```PowerShell
Get-Content -Path "C:\path\to\file.txt"
```

### Вивід:
```
Привіт, Україно!
Додаємо текст
```

## Поглиблений розгляд:
У минулому для запису в текстові файли часто користувались скриптовими мовами як Perl або Bash. PowerShell дозволяє працювати в Windows-орієнтованому середовищі з більшою зручністю і безпекою. Альтернативи `Set-Content` і `Add-Content` включають використання методів `.NET`, наприклад, `[System.IO.File]::WriteAllText()`, які можуть бути більш гнучкими при складних завданнях. Що стосується імплементації, PowerShell оперує об'єктами, а не простим текстом, тому при запису даних у файл, він може автоматично впорядковувати їх як об'єктну структуру (наприклад, у форматі JSON або XML).

## Дивіться також:
- [Офіційна документація PowerShell](https://docs.microsoft.com/powershell/)
- [Довідник з командлетів Set-Content](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/set-content)
- [Довідник з командлетів Add-Content](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/add-content)
- [Довідник з командлетів Get-Content](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/get-content)
- [Проект PowerShell на GitHub](https://github.com/PowerShell/PowerShell)
