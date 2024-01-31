---
title:                "JSON 다루기"
date:                  2024-01-19
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
JSON은 데이터를 저장하고 전송하는 경량 포맷입니다. 프로그래머는 정보 교환과 설정 파일을 다루기 위해 JSON을 사용해요.

## How to: (방법)
Fish Shell에서 JSON 다루기 위해 'jq' 툴을 주로 사용해요. 아래 예시를 보세요.

```Fish Shell
# JSON 객체 만들기
echo '{"name": "Yuna", "age": 25}' | jq '.'

# 출력:
# {
#   "name": "Yuna",
#   "age": 25
# }

# 특정 필드 가져오기
echo '{"name": "Yuna", "age": 25}' | jq '.name'

# 출력:
# "Yuna"
```

## Deep Dive (심층 분석)
JSON(JavaScript Object Notation)은 2000년대 초반에 개발됐어요. XML과 비교해 더 단순하고 읽기 쉬워 인기를 끌었죠. Fish Shell에서는 'jq'와 같은 외부 툴 없이 내장된 기능으로 JSON을 처리하기 어렵습니다. 그래서 'jq'가 매우 유용해요. 'jq'는 다양한 커맨드와 필터로 JSON 데이터를 매끄럽게 변경하고 쿼리할 수 있게 해줘요.

## See Also (관련 자료)
- jq 공식 문서: https://stedolan.github.io/jq/manual/
- Fish Shell 공식 문서: https://fishshell.com/docs/current/index.html
- JSON 공식 웹사이트: https://www.json.org/json-en.html
