---
title:                "문자열 보간하기"
aliases:
- ko/fish-shell/interpolating-a-string.md
date:                  2024-01-20T17:50:50.782053-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 보간이란, 문자열 내에 변수나 표현식의 값을 삽입하는 기법입니다. 프로그래머들은 원하는 문자열 패턴을 더 쉽고 정확하게 생성하기 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
Fish Shell에서 문자열 보간을 사용하는 것은 단순합니다. 변수 값이나 명령어 출력을 문자열과 결합하려면 $ 기호를 사용하세요. 아래 예시를 참조하세요.

```Fish Shell
set name "세상"
echo "안녕, $name!" # 변수 보간
# 출력: 안녕, 세상!

set greeting (echo "안녕")
echo "$greeting, 다시 만났군요!" # 명령어 치환
# 출력: 안녕, 다시 만났군요!
```

## Deep Dive (심층 탐구)
Fish Shell에서의 문자열 보간은 타 스크립팅 언어에서도 흔히 볼 수 있는 기능입니다. 예를 들어, Bash에서는 `$`를 이용하거나 `${}`로 변수를 감싸서 사용합니다. Fish는 따로 감싸지 않고 바로 사용하는 간결함이 특징입니다.

과거에 명령어 치환은 백틱(`)으로 이루어졌지만, 이는 가독성이 떨어지고 중첩 사용에서 불편함이 있어서 Fish와 다른 현대 쉘들은 $(...) 형식을 사용합니다. 

Fish Shell의 구현을 살펴보면, 문자열 보간은 명령어 치환과 변수 치환 모두 `eval` 함수 없이 내부적으로 처리되어, 실수로 코드를 실행하는 일 없이 보안을 강화합니다.

## See Also (더 보기)
- 공식 Fish Shell 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Fish Shell GitHub Repository: [https://github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- 문자열 보간에 대한 더 깊은 토론: [https://fishshell.com/docs/current/tutorial.html#tut_quoting](https://fishshell.com/docs/current/tutorial.html#tut_quoting)
