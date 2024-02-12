---
title:                "TOML과 함께 일하기"
aliases:
- /ko/vba/working-with-toml/
date:                  2024-02-01T22:06:44.250042-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

TOML은 Tom's Obvious, Minimal Language의 약자로, 주로 설정 파일에 사용되는 데이터 직렬화 형식입니다. 프로그래머들은 TOML을 그 읽기 쉬움과 데이터 구조로의 쉬운 매핑으로 인해, 다양한 프로그래밍 환경을 포함하여 Visual Basic for Applications (VBA)에서 응용 프로그램 구성을 간단하게 할 수 있도록 활용합니다.

## 방법:

VBA에서 TOML을 다루는 것은 TOML 파일을 파싱하여 설정 또는 설정을 VBA 프로젝트에 읽어 들이는 것을 포함합니다. VBA는 내장된 TOML 지원이 없기 때문에, 일반적으로 파서를 사용하거나 TOML 데이터를 VBA가 쉽게 작업할 수 있는 형식, 예를 들어 JSON이나 XML로 변환합니다. 다음은 간단한 TOML 구성 파일을 수동으로 파싱하는 방법입니다:

1. **샘플 TOML 파일** (`config.toml`):
```
title = "TOML 예제"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **TOML 파싱을 위한 VBA 코드**:

TOML 내용이 문자열 변수 `tomlStr`로 읽혀진다고 가정하면, 다음 VBA 코드는 `[database]` 섹션을 파싱하는 단순한 방법을 보여줍니다:

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
    
    '파싱된 데이터에 접근하는 예
    Debug.Print "데이터베이스 서버: "; config("database")("server")
End Function
```

3. **샘플 출력** (즉시 창):
```
데이터베이스 서버: 192.168.1.1
```

## 깊이 있는 탐구

개발자 커뮤니티에서 TOML의 실제 수용은 보다 간단하고 인간이 읽기 쉬운 구성 파일을 향한 경향을 보여주며, 이전에 널리 사용되던 XML과 대조됩니다. TOML의 디자인 철학은 명확한 의미 체계를 강조하고 최소한의 오버헤드로 쉽게 파싱을 목표로 합니다. VBA에서 TOML을 직접 다루는 것은 수동 파싱 또는 TOML을 보다 VBA 친화적인 형식으로 변환하기 위해 외부 도구를 활용하는 것을 포함합니다. 이러한 수동 파싱 방법이 기본 접근 방식을 보여주는 동안, 외부 라이브러리나 중간 형식 이용은 더욱 견고하고 오류에 강한 파싱 전략을 제공할 수 있습니다. VBA가 Microsoft Office와 광범위하게 통합되어 있기 때문에, TOML을 JSON으로 변환하고 VBA의 기본 JSON 파싱 기능(해당되는 경우) 또는 타사 JSON 파서를 사용하는 것이 더 효율적인 작업 흐름을 제공할 수 있습니다. 게다가, 데이터 직렬화 형식이 지속적으로 발전함에 따라, TOML처럼 인간의 가독성을 강조하지만 복잡성과 유연성 측면에서 다른 절충안을 제공하는 YAML도 고려해야 합니다.
