---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:47.402665-07:00
description: "Bash \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\
  \uB294 \uAC83\uC740 \uCEE4\uB9E8\uB4DC \uB77C\uC778\uC5D0\uC11C \uC9C1\uC811 JSON\
  \ \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uACE0, \uCD94\uCD9C\uD558\uBA70, \uC870\
  \uC791\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC6F9 API \uBC0F \uD604\uB300 \uB370\uC774\
  \uD130 \uAD50\uD658 \uD3EC\uB9F7\uACFC \uC258 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC6D0\
  \uD65C\uD558\uAC8C \uD1B5\uD569\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\
  \uD558\uBA70, \uC774\uB294 Bash \uC2A4\uD06C\uB9BD\uD305\uC744\u2026"
lastmod: '2024-03-13T22:44:55.512885-06:00'
model: gpt-4-0125-preview
summary: "Bash \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB294\
  \ \uAC83\uC740 \uCEE4\uB9E8\uB4DC \uB77C\uC778\uC5D0\uC11C \uC9C1\uC811 JSON \uB370\
  \uC774\uD130\uB97C \uD30C\uC2F1\uD558\uACE0, \uCD94\uCD9C\uD558\uBA70, \uC870\uC791\
  \uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC885\uC885 \uC6F9 API \uBC0F \uD604\uB300 \uB370\uC774\uD130\
  \ \uAD50\uD658 \uD3EC\uB9F7\uACFC \uC258 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC6D0\uD65C\
  \uD558\uAC8C \uD1B5\uD569\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD558\
  \uBA70, \uC774\uB294 Bash \uC2A4\uD06C\uB9BD\uD305\uC744\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
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
