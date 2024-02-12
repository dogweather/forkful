---
title:                "JSON과 함께 일하기"
aliases:
- /ko/bash/working-with-json/
date:                  2024-02-03T19:21:47.402665-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜?
Bash 프로그래밍에서 JSON을 다루는 것은 커맨드 라인에서 직접 JSON 데이터를 파싱하고, 추출하며, 조작하는 것을 포함합니다. 프로그래머들은 종종 웹 API 및 현대 데이터 교환 포맷과 쉘 스크립트를 원활하게 통합하기 위해 이를 수행하며, 이는 Bash 스크립팅을 JSON이 많은 에코시스템에서 더 강력하고 관련 있게 만듭니다.

## 어떻게:
Bash 자체는 내장된 JSON 파싱 기능이 없지만, `jq`는 이러한 격차를 메우는 강력한 커맨드 라인 JSON 프로세서입니다. 사용 방법은 다음과 같습니다:

**JSON 파일 읽기:**

샘플 `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

JSON 파일에서 이름을 읽고 추출하려면:
```bash
jq '.name' data.json
```
출력:
```
"Jane Doe"
```

**JSON 데이터 수정하기:**

도시를 "Los Angeles"로 업데이트하고 파일에 다시 쓰려면:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**변수에서 JSON 파싱하기:**

Bash 변수에 JSON이 있으면, `jq`는 여전히 그것을 처리할 수 있습니다:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
출력:
```
"John Doe"
```

**배열 다루기:**

JSON에서 항목 배열이 주어진 경우:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

두 번째 항목을 추출하려면 (인덱스는 0부터 시작):
```bash
jq '.items[1]' data.json
```
출력:
```
"banana"
```

더 복잡한 작업과 필터링을 위해, `jq`는 온라인으로 사용 가능한 종합적인 매뉴얼과 튜토리얼을 제공하여, 당신의 모든 Bash/JSON 요구에 다재다능한 도구가 됩니다.
