---
title:                "JSON 다루기"
date:                  2024-01-19
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON은 데이터를 저장하고 전송할 때 쓰이는 텍스트 포맷입니다. 프로그래머들은 설정, API 응답 등의 데이터를 쉽고 효율적으로 다루기 위해 JSON을 사용합니다.

## How to:
Bash에서 JSON 다루는 법을 보여줍니다. `jq`라는 툴을 씁니다.

```Bash
# JSON 파일 읽기
echo '{"name": "Yeon", "age": 25}' | jq '.'

# 특정 필드 추출하기
echo '{"name": "Yeon", "age": 25}' | jq '.name'

# Sample Output: "Yeon"

# 배열에 있는 요소들 추출하기
echo '{"users": [{"name": "Yeon"}, {"name": "Jin"}]}' | jq '.users[].name'

# Sample Output: 
# "Yeon"
# "Jin"
```

## Deep Dive
JSON (JavaScript Object Notation)은 2001년에 개발되었습니다. XML, YAML 같은 대안들도 있지만 JSON은 가독성과 유연성 때문에 인기가 많습니다. `jq`는 C로 쓰여진 경량의 명령줄 JSON 프로세서로, 다양한 JSON 데이터 조작을 지원합니다.

## See Also
- [jq 공식 문서](https://stedolan.github.io/jq/manual/)
- [JSON에 대한 MDN 설명](https://developer.mozilla.org/docs/Learn/JavaScript/Objects/JSON)
- [Bash에서 JSON 데이터 처리하는 다른 방법들](https://www.baeldung.com/linux/jq-command-json)
