---
title:                "Работа с TOML"
aliases:
- /ru/vba/working-with-toml/
date:                  2024-02-01T22:06:38.275266-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

TOML, что означает "Ясный, Минимальный Язык Тома", представляет собой формат сериализации данных, в основном используемый для файлов конфигурации. Программисты используют TOML за его читаемость и легкость сопоставления с структурами данных, что обеспечивает прямую настройку приложений в различных программных средах, включая Visual Basic для приложений (VBA).

## Как это сделать:

Работа с TOML в VBA включает разбор файла TOML для чтения конфигураций или настроек в ваш проект VBA. VBA не имеет встроенной поддержки для TOML, поэтому обычно используется парсер или преобразование данных TOML в формат, с которым VBA может легко работать, например, JSON или XML. Вот как вручную разобрать простой конфигурационный файл TOML:

1. **Пример файла TOML** (`config.toml`):
```
title = "Пример TOML"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **Код VBA для разбора TOML**:

Предполагая, что содержимое TOML считано в строковую переменную `tomlStr`, следующий код VBA демонстрирует упрощенный подход к разбору секции `[database]`:

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
    
    'Пример доступа к разобранным данным
    Debug.Print "Сервер базы данных: "; config("database")("server")
End Function
```

3. **Пример вывода** (окно немедленных сообщений):
```
Сервер базы данных: 192.168.1.1
```

## Глубокое погружение

Практическое признание TOML в сообществе разработчиков демонстрирует тенденцию к более простым, понятным для человека файлам конфигурации, что контрастирует с ранее распространенными XML. Философия проектирования TOML акцентирует внимание на четких семантиках и стремится к простому разбору с минимальными накладными расходами. В VBA прямая работа с TOML включает в себя ручной разбор или использование внешних инструментов для преобразования TOML в более подходящий для VBA формат из-за отсутствия встроенной поддержки. Хотя этот метод ручного разбора демонстрирует основной подход, использование внешних библиотек или промежуточных форматов, таких как JSON, может предложить более надежные и устойчивые к ошибкам стратегии разбора. Учитывая широкую интеграцию VBA с Microsoft Office, преобразование TOML в JSON и использование встроенных возможностей VBA для разбора JSON (где это применимо) или сторонних парсеров JSON могут обеспечить более эффективный рабочий процесс. Кроме того, с постоянным развитием форматов сериализации данных, программистам также следует рассмотреть YAML, который, подобно TOML, акцентирует внимание на читаемости для человека, но предлагает разные компромиссы с точки зрения сложности и гибкости.
