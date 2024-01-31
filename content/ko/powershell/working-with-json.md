---
title:                "JSON 다루기"
date:                  2024-01-19
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON은 데이터 저장 및 교환 포맷입니다. 프로그래머들은 이해하기 쉽고, 다양한 언어에서 쉽게 다룰 수 있어서 JSON을 자주 사용합니다.

## How to:
JSON 데이터 읽기, 쓰기, 변환을 PowerShell에서 쉽게 할 수 있습니다. 아래는 예제입니다:

```PowerShell
# JSON 문자열을 오브젝트로 변환하기
$jsonString = '{"name": "Kim", "age": 30}'
$userObject = $jsonString | ConvertFrom-Json
$userObject.name  # Kim 출력

# 오브젝트를 JSON으로 변환하기
$userObject = [PSCustomObject]@{ name = "Lee"; age = 25 }
$jsonString = $userObject | ConvertTo-Json
$jsonString  # {"name":"Lee","age":25} 출력
```

## Deep Dive
JSON (JavaScript Object Notation)은 2001년부터 웹에서 데이터 교환 포맷으로 널리 사용되었습니다. XML과 같은 대안들도 있지만, JSON이 더 가볍고, 코드 가독성이 높습니다. PowerShell에서는 `ConvertFrom-Json`과 `ConvertTo-Json` cmdlet을 통해 JSON 작업을 할 수 있고, 처리 속도도 빠릅니다.

## See Also
- [ConvertFrom-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json)
- [ConvertTo-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json)
- [PowerShell Gallery JSON modules](https://www.powershellgallery.com/packages?q=JSON)
