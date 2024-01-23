---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜 사용하나요?)
문자열 대문자화는 문자열의 모든 문자를 대문자로 바꾸는 것입니다. 이를 통해 로봇이 아닌 사람이 읽기 쉬운 텍스트로 변환할 수 있거나, 데이터 정규화에 활용됩니다.

## How to (어떻게 하나요?)
Fish Shell에서 문자열을 대문자로 만드는 방법을 아래 코드 블록에서 확인할 수 있습니다.

```Fish Shell
function capitalize_string
    set -l str $argv
    echo $str | string to-upper
end

set my_string "fish shell은 재미있습니다."
capitalize_string $my_string
```

출력 결과:
```
FISH SHELL은 재미있습니다.
```

## Deep Dive (심층 분석)
문자열 대문자화는 오래된 컴퓨팅 개념입니다. 초기 프로그래밍 언어부터 사용자의 입력을 일관된 형태로 처리하기 위해 쓰여왔죠. Fish Shell은 여러 내장 명령어로 대문자화를 쉽게 만들어줍니다. `string` 명령어는 Fish 2.3.0부터 추가되었으며 문자열 조작을 위한 강력한 도구입니다. Fish에서 대문자화 이외에도, `string to-lower`, `string trim`, `string replace` 등 다양한 문자열 조작을 지원합니다.

대안으로는 `awk`, `tr`, `sed` 같은 유닉스 표준 도구를 이용할 수 있지만, Fish 내장 기능이 더 간결하고 사용하기 쉽습니다. 특히 `string to-upper`는 유니코드 문자를 올바르게 처리하며, 이는 다국어를 지원하는 현대 애플리케이션에서 중요한 특징입니다.

## See Also (참고자료)
- Fish Shell 공식 문서(`string` 명령어): https://fishshell.com/docs/current/cmds/string.html
- Fish Shell GitHub 페이지: https://github.com/fish-shell/fish-shell
- 유닉스 문자 조작에 대한 더 넓은 배경 지식: https://en.wikipedia.org/wiki/String_(computer_science)
