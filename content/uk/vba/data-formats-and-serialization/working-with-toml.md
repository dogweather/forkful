---
title:                "Працюємо з TOML"
aliases: - /uk/vba/working-with-toml.md
date:                  2024-02-01T22:07:25.719401-07:00
model:                 gpt-4-0125-preview
simple_title:         "Працюємо з TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

TOML, що розшифровується як Tom's Obvious, Minimal Language (очевидна, мінімальна мова Тома), є форматом серіалізації даних, що переважно використовується для файлів конфігурації. Програмісти використовують TOML за його читабельність та легке відображення на структури даних, що дозволяє прямолінійно конфігурувати додатки в різних програмувальних оточеннях, включаючи Visual Basic для додатків (VBA).

## Як це зробити:

Робота з TOML у VBA передбачає парсинг файлу TOML для читання конфігурацій або налаштувань у ваш проєкт VBA. VBA не має вбудованої підтримки для TOML, тому зазвичай ви використовуватимете парсер або перетворюватимете дані TOML у формат, з яким VBA може легко працювати, наприклад, JSON або XML. Ось як вручну проаналізувати простий конфігураційний файл TOML:

1. **Приклад файлу TOML** (`config.toml`):
```
title = "Приклад TOML"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **Код VBA для парсингу TOML**:

Припустимо, що вміст TOML прочитано у змінну рядка `tomlStr`, такий код VBA демонструє спрощений підхід для парсингу секції `[database]`:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'Приклад доступу до проаналізованих даних
    Debug.Print "Сервер бази даних: "; config("database")("server")
End Function
```

3. **Приклад виводу** (вікно негайних результатів):
```
Сервер бази даних: 192.168.1.1
```

## Поглиблений огляд

Практичне прийняття TOML в спільноті розробників продемонструвало тенденцію до більш простих, читабельних файлів конфігурації, стоячи у контрасті з колись поширеним XML. Філософія дизайну TOML підкреслює чіткі семантичні правила і має за мету забезпечити простий парсинг з мінімальними витратами. У VBA робота з TOML безпосередньо передбачає ручний парсинг або використання зовнішніх інструментів для перетворення TOML у більш підходящий для VBA формат через відсутність нативної підтримки. Хоча цей метод ручного парсингу демонструє основний підхід, використання зовнішніх бібліотек або проміжних форматів, таких як JSON, може запропонувати більш надійні та помилково-стійкі стратегії парсингу. Враховуючи широке інтегрування VBA з Microsoft Office, перетворення TOML у JSON та використання нативних можливостей VBA для парсингу JSON (де це застосовно) або сторонніх парсерів JSON може забезпечити більш плавну роботу. Крім того, з постійним розвитком форматів серіалізації даних, програмісти також повинні розглядати YAML, який, як і TOML, наголошує на читабельності для людини, але пропонує інші компроміси з точки зору складності та гнучкості.
