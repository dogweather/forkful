---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:34.414430-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:55.577918-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
