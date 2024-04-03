---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:47.402665-07:00
description: "\uC5B4\uB5BB\uAC8C: Bash \uC790\uCCB4\uB294 \uB0B4\uC7A5\uB41C JSON\
  \ \uD30C\uC2F1 \uAE30\uB2A5\uC774 \uC5C6\uC9C0\uB9CC, `jq`\uB294 \uC774\uB7EC\uD55C\
  \ \uACA9\uCC28\uB97C \uBA54\uC6B0\uB294 \uAC15\uB825\uD55C \uCEE4\uB9E8\uB4DC \uB77C\
  \uC778 JSON \uD504\uB85C\uC138\uC11C\uC785\uB2C8\uB2E4. \uC0AC\uC6A9 \uBC29\uBC95\
  \uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4: **JSON \uD30C\uC77C \uC77D\uAE30\
  :** \uC0D8\uD50C `data.json`."
lastmod: '2024-03-13T22:44:55.512885-06:00'
model: gpt-4-0125-preview
summary: "Bash \uC790\uCCB4\uB294 \uB0B4\uC7A5\uB41C JSON \uD30C\uC2F1 \uAE30\uB2A5\
  \uC774 \uC5C6\uC9C0\uB9CC, `jq`\uB294 \uC774\uB7EC\uD55C \uACA9\uCC28\uB97C \uBA54\
  \uC6B0\uB294 \uAC15\uB825\uD55C \uCEE4\uB9E8\uB4DC \uB77C\uC778 JSON \uD504\uB85C\
  \uC138\uC11C\uC785\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
