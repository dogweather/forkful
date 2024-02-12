---
title:                "JSON과 함께 일하기"
aliases:
- /ko/powershell/working-with-json.md
date:                  2024-02-03T19:23:34.414430-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

PowerShell이 JSON(JavaScript Object Notation)과 통합되는 것은 JSON 데이터를 파싱(읽기)하고 생성(쓰기)하는 것과 관련이 있습니다. 이는 웹상의 데이터 교환을 위한 일반적인 형식입니다. 프로그래머들은 가벼운 특성과 언어 독립적인 성격 때문에, 웹 API와 상호 작용하거나 설정 파일을 다루거나, 다양한 언어 및 플랫폼 간 데이터 교환을 용이하게 하기 위해 JSON을 사용합니다.

## 방법:

### JSON 파싱

PowerShell에서 JSON을 읽거나 파싱하기 위해서는 `ConvertFrom-Json` cmdlet을 사용할 수 있습니다. 주어진 JSON 문자열에 대해, 이 cmdlet은 PowerShell 객체로 변환합니다.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

샘플 출력:

```
John Doe
```

이 예제는 단순한 JSON 문자열을 파싱하여 결과 객체의 속성에 접근하는 방법을 보여줍니다.

### JSON 생성

PowerShell 객체에서 JSON을 생성하기 위해서는 `ConvertTo-Json` cmdlet을 사용할 수 있습니다. 이는 웹 서비스로 데이터를 보내거나 설정 파일에 저장하기 위한 준비과정에서 유용합니다.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

샘플 출력:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

이 코드 스니펫은 PowerShell 객체를 생성한 다음 JSON 문자열로 변환합니다.
