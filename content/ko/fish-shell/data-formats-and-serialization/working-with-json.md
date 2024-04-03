---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:50.956016-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: Fish Shell\uC740 \uADF8 \uC790\uCCB4\
  \uB85C\uB294 JSON\uC744 \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\uD558\uAE30 \uC704\
  \uD55C \uB0B4\uC7A5 \uC720\uD2F8\uB9AC\uD2F0\uB97C \uAC16\uACE0 \uC788\uC9C0 \uC54A\
  \uC2B5\uB2C8\uB2E4. \uD558\uC9C0\uB9CC, `jq` \uAC19\uC740 \uD0C0\uC0AC \uB3C4\uAD6C\
  \uC640\uC758 \uC190\uC26C\uC6B4 \uD1B5\uD569\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . `jq`\uB294 \uAC15\uB825\uD558\uACE0 \uB2E4\uC7AC\uB2E4\uB2A5\uD55C \uCEE4\uB9E8\
  \uB4DC \uB77C\uC778 JSON \uD504\uB85C\uC138\uC11C\uB85C, \uAC04\uB2E8\uD558\uACE0\
  \ \uD45C\uD604\uB825\uC774 \uB6F0\uC5B4\uB09C\u2026"
lastmod: '2024-03-13T22:44:55.886692-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\uC740 \uADF8 \uC790\uCCB4\uB85C\uB294 JSON\uC744 \uD30C\uC2F1\
  \uD558\uACE0 \uC0DD\uC131\uD558\uAE30 \uC704\uD55C \uB0B4\uC7A5 \uC720\uD2F8\uB9AC\
  \uD2F0\uB97C \uAC16\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 어떻게 하나:
Fish Shell은 그 자체로는 JSON을 파싱하고 생성하기 위한 내장 유틸리티를 갖고 있지 않습니다. 하지만, `jq` 같은 타사 도구와의 손쉬운 통합을 제공합니다. `jq`는 강력하고 다재다능한 커맨드 라인 JSON 프로세서로, 간단하고 표현력이 뛰어난 언어를 사용하여 구조화된 데이터를 슬라이스하고, 필터링하고, 매핑하고, 변환할 수 있게 해줍니다.

### jq로 JSON 파싱하기
`jq`를 사용해 JSON 파일을 파싱하고 데이터를 추출하는 방법:

```fish
# 'data.json'이라는 이름의 JSON 파일이 있고, 내용이 {"name":"Fish Shell","version":"3.4.0"} 라고 가정합니다.
cat data.json | jq '.name'
# 출력 예시
"Fish Shell"
```

### jq로 JSON 생성하기
쉘 변수나 출력물에서 JSON 콘텐츠 생성:

```fish
# 변수에서 JSON 객체 생성하기
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# 출력 예시
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### JSON 컬렉션 필터링하기
`versions.json`이라는 이름의 파일에 객체의 JSON 배열이 있다고 가정해봅시다:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
이 배열에서 안정적인(Stable) 버전만 필터링하기 위해:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# 출력 예시
"3.1.2"
"3.4.0"
```

제공된 예시들은 Fish Shell에서 `jq`와 통합하여 JSON 작업을 수행하는 힘을 보여줍니다. 이러한 도구를 활용하는 것은 쉘 경험을 풍부하게 하여, 현대 데이터 포맷을 처리하는 데 있어 견고한 환경을 만들어냅니다.
